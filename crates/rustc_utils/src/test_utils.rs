//! Running rustc and Flowistry in tests.

use std::{
  fmt::Debug, fs, hash::Hash, io, panic, path::Path, process::Command, sync::LazyLock,
};

use anyhow::{anyhow, ensure, Context, Result};
use log::debug;
use rustc_borrowck::consumers::BodyWithBorrowckFacts;
use rustc_data_structures::fx::{FxHashMap as HashMap, FxHashSet as HashSet};
use rustc_hir::{BodyId, ItemKind};
use rustc_middle::{
  mir::{Body, HasLocalDecls, Local, Place},
  ty::TyCtxt,
};
use rustc_span::source_map::FileLoader;
use rustc_target::abi::{FieldIdx, VariantIdx};

use crate::{
  mir::borrowck_facts,
  source_map::{
    filename::{Filename, FilenameIndex},
    find_bodies::find_enclosing_bodies,
    range::{BytePos, ByteRange, CharPos, CharRange, ToSpan},
  },
  BodyExt, PlaceExt,
};

pub struct StringLoader(pub String);
impl FileLoader for StringLoader {
  fn file_exists(&self, _: &Path) -> bool {
    true
  }

  fn read_file(&self, _: &Path) -> io::Result<String> {
    Ok(self.0.clone())
  }

  fn read_binary_file(&self, path: &Path) -> io::Result<Vec<u8>> {
    fs::read(path)
  }
}

static SYSROOT: LazyLock<String> = LazyLock::new(|| {
  let rustc_output = Command::new("rustc")
    .args(["--print", "sysroot"])
    .output()
    .unwrap()
    .stdout;
  String::from_utf8(rustc_output).unwrap().trim().to_owned()
});

pub const DUMMY_FILE_NAME: &str = "dummy.rs";

thread_local! {
  pub static DUMMY_FILE: FilenameIndex = Filename::intern(DUMMY_FILE_NAME);
  pub static DUMMY_BYTE_RANGE: ByteRange = DUMMY_FILE.with(|filename| ByteRange {
    start: BytePos(0),
    end: BytePos(0),
    filename: *filename,
  });
  pub static DUMMY_CHAR_RANGE: CharRange = DUMMY_FILE.with(|filename| CharRange {
    start: CharPos { line: 0, column: 0 },
    end: CharPos { line: 0, column: 0 },
    filename: *filename,
  });
}

pub fn compile_body_with_range(
  input: impl Into<String>,
  compute_target: impl FnOnce() -> ByteRange + Send,
  callback: impl for<'tcx> FnOnce(TyCtxt<'tcx>, BodyId, &BodyWithBorrowckFacts<'tcx>, ByteRange)
    + Send,
) {
  compile(input, |tcx| {
    let target = compute_target();
    let body_id = find_enclosing_bodies(tcx, target.to_span(tcx).unwrap())
      .next()
      .unwrap();
    let def_id = tcx.hir().body_owner_def_id(body_id);
    let body_with_facts = borrowck_facts::get_body_with_borrowck_facts(tcx, def_id);
    debug!("{}", body_with_facts.body.to_string(tcx).unwrap());

    callback(tcx, body_id, body_with_facts, target);
  })
}

pub fn compile_body(
  input: impl Into<String>,
  callback: impl for<'tcx> FnOnce(TyCtxt<'tcx>, BodyId, &BodyWithBorrowckFacts<'tcx>) + Send,
) {
  compile(input, |tcx| {
    let hir = tcx.hir();
    let body_id = hir
      .items()
      .filter_map(|id| match hir.item(id).kind {
        ItemKind::Fn(_, _, body) => Some(body),
        _ => None,
      })
      .next()
      .unwrap();

    let def_id = tcx.hir().body_owner_def_id(body_id);
    let body_with_facts = borrowck_facts::get_body_with_borrowck_facts(tcx, def_id);
    debug!("{}", body_with_facts.body.to_string(tcx).unwrap());

    callback(tcx, body_id, body_with_facts);
  })
}

pub fn compile(input: impl Into<String>, callback: impl FnOnce(TyCtxt<'_>) + Send) {
  let mut callbacks = TestCallbacks {
    callback: Some(callback),
  };
  let args = format!(
    "rustc {DUMMY_FILE_NAME} --crate-type lib --edition=2021 -Z identify-regions -Z mir-opt-level=0 -Z maximal-hir-to-mir-coverage --allow warnings --sysroot {}",
    &*SYSROOT
  );
  let args = args.split(' ').map(|s| s.to_string()).collect::<Vec<_>>();

  rustc_driver::catch_fatal_errors(|| {
    let mut compiler = rustc_driver::RunCompiler::new(&args, &mut callbacks);
    compiler.set_file_loader(Some(Box::new(StringLoader(input.into()))));
    compiler.run()
  })
  .unwrap()
  .unwrap();
}

struct TestCallbacks<Cb> {
  callback: Option<Cb>,
}

impl<Cb> rustc_driver::Callbacks for TestCallbacks<Cb>
where
  Cb: FnOnce(TyCtxt<'_>),
{
  fn config(&mut self, config: &mut rustc_interface::Config) {
    config.override_queries = Some(borrowck_facts::override_queries);
  }

  fn after_expansion<'tcx>(
    &mut self,
    _compiler: &rustc_interface::interface::Compiler,
    queries: &'tcx rustc_interface::Queries<'tcx>,
  ) -> rustc_driver::Compilation {
    queries.global_ctxt().unwrap().enter(|tcx| {
      let callback = self.callback.take().unwrap();
      callback(tcx);
    });
    rustc_driver::Compilation::Stop
  }
}

pub type RangeMap = HashMap<&'static str, Vec<ByteRange>>;

pub fn parse_ranges(
  src: impl AsRef<str>,
  delimiters: impl AsRef<[(&'static str, &'static str)]>,
) -> Result<(String, RangeMap)> {
  let src = src.as_ref();
  let delimiters = delimiters.as_ref();

  let mut in_idx = 0;
  let mut out_idx = 0;
  let mut buf = Vec::new();
  let bytes = src.bytes().collect::<Vec<_>>();
  let mut stack = vec![];

  let (opens, closes): (Vec<_>, Vec<_>) = delimiters.iter().copied().unzip();
  let mut ranges: HashMap<_, Vec<_>> = HashMap::default();

  macro_rules! check_token {
    ($tokens:expr) => {
      $tokens
        .iter()
        .find(|t| {
          in_idx + t.len() <= bytes.len()
            && t.as_bytes() == &bytes[in_idx .. in_idx + t.len()]
        })
        .map(|t| *t)
    };
  }

  while in_idx < bytes.len() {
    if let Some(open) = check_token!(opens) {
      stack.push((out_idx, open));
      in_idx += open.len();
      continue;
    }

    if let Some(close) = check_token!(closes) {
      let (start, delim) = stack
        .pop()
        .with_context(|| anyhow!("Missing open delimiter for \"{close}\""))?;
      let range = DUMMY_FILE.with(|filename| ByteRange {
        start: BytePos(start),
        end: BytePos(out_idx),
        filename: *filename,
      });
      ranges.entry(delim).or_default().push(range);
      in_idx += close.len();
      continue;
    }

    buf.push(bytes[in_idx]);
    in_idx += 1;
    out_idx += 1;
  }

  ensure!(stack.is_empty(), "Unclosed delimiters: {stack:?}");

  let prog_clean = String::from_utf8(buf)?;
  Ok((prog_clean, ranges))
}

pub fn color_ranges(prog: &str, all_ranges: Vec<(&str, &HashSet<ByteRange>)>) -> String {
  let mut new_tokens = all_ranges
    .iter()
    .flat_map(|(_, ranges)| {
      ranges.iter().flat_map(|range| {
        let contained = all_ranges.iter().any(|(_, ranges)| {
          ranges.iter().any(|other| {
            range != other && other.start.0 <= range.end.0 && range.end.0 < other.end.0
          })
        });
        let end_marker = if contained { "]" } else { "\x1B[0m]" };
        [("[\x1B[31m", range.start), (end_marker, range.end)]
      })
    })
    .collect::<Vec<_>>();
  new_tokens.sort_by_key(|(_, i)| -(i.0 as isize));

  let mut output = prog.to_owned();
  for (s, i) in new_tokens {
    output.insert_str(i.0, s);
  }

  output
}

pub fn fmt_ranges(prog: &str, s: &HashSet<ByteRange>) -> String {
  textwrap::indent(&color_ranges(prog, vec![("", s)]), "  ")
}

pub fn compare_ranges(
  expected: HashSet<ByteRange>,
  actual: HashSet<ByteRange>,
  prog: &str,
) {
  let missing = &expected - &actual;
  let extra = &actual - &expected;

  let check = |s: HashSet<ByteRange>, message: &str| {
    if s.len() > 0 {
      println!("Expected ranges:\n{}", fmt_ranges(prog, &expected));
      println!("Actual ranges:\n{}", fmt_ranges(prog, &actual));
      panic!("{message} ranges:\n{}", fmt_ranges(prog, &s));
    }
  };

  check(missing, "Analysis did NOT have EXPECTED");
  check(extra, "Actual DID have UNEXPECTED");
}

pub struct Placer<'a, 'tcx> {
  tcx: TyCtxt<'tcx>,
  body: &'a Body<'tcx>,
  local_map: HashMap<String, Local>,
}

impl<'a, 'tcx> Placer<'a, 'tcx> {
  pub fn new(tcx: TyCtxt<'tcx>, body: &'a Body<'tcx>) -> Self {
    let local_map = body.debug_info_name_map();
    Placer {
      tcx,
      body,
      local_map,
    }
  }

  pub fn local(&self, name: &str) -> PlaceBuilder<'a, 'tcx> {
    PlaceBuilder {
      place: Place::from_local(self.local_map[name], self.tcx),
      body: self.body,
      tcx: self.tcx,
    }
  }
}

#[derive(Copy, Clone)]
pub struct PlaceBuilder<'a, 'tcx> {
  tcx: TyCtxt<'tcx>,
  body: &'a Body<'tcx>,
  place: Place<'tcx>,
}

impl<'a, 'tcx> PlaceBuilder<'a, 'tcx> {
  pub fn field(mut self, i: usize) -> Self {
    let f = FieldIdx::from_usize(i);
    let ty = self
      .place
      .ty(self.body.local_decls(), self.tcx)
      .field_ty(self.tcx, f);
    self.place = self.tcx.mk_place_field(self.place, f, ty);
    self
  }

  pub fn deref(mut self) -> Self {
    self.place = self.tcx.mk_place_deref(self.place);
    self
  }

  pub fn downcast(mut self, i: usize) -> Self {
    let ty = self.place.ty(self.body.local_decls(), self.tcx).ty;
    let adt_def = ty.ty_adt_def().unwrap();
    let v = VariantIdx::from_usize(i);
    self.place = self.tcx.mk_place_downcast(self.place, adt_def, v);
    self
  }

  pub fn index(mut self, i: usize) -> Self {
    self.place = self.tcx.mk_place_index(self.place, Local::from_usize(i));
    self
  }

  pub fn mk(self) -> Place<'tcx> {
    self.place
  }
}

pub fn compare_sets<T: PartialEq + Eq + Clone + Hash + Debug>(
  expected: impl IntoIterator<Item = T>,
  actual: impl IntoIterator<Item = T>,
) {
  let expected = expected.into_iter().collect::<HashSet<_>>();
  let actual = actual.into_iter().collect::<HashSet<_>>();

  let missing = &expected - &actual;
  let extra = &actual - &expected;

  let check = |s: HashSet<T>, message: &str| {
    if s.len() > 0 {
      println!(
        "Expected:\n{}",
        textwrap::indent(&format!("{expected:#?}"), "  ")
      );
      println!(
        "Actual:\n{}",
        textwrap::indent(&format!("{actual:#?}"), "  ")
      );
      panic!(
        "{message} ranges:\n{}",
        textwrap::indent(&format!("{s:#?}"), "  ")
      );
    }
  };

  check(missing, "Result did NOT have EXPECTED");
  check(extra, "Result DID have UNEXPECTED");
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn test_parse_ranges() {
    DUMMY_FILE.with(|filename| {
      let s = "`[`[f]`oo]`";
      let (clean, ranges) = parse_ranges(s, vec![("`[", "]`")]).unwrap();
      assert_eq!(clean, "foo");
      assert_eq!(ranges["`["], vec![
        ByteRange {
          start: BytePos(0),
          end: BytePos(1),
          filename: *filename,
        },
        ByteRange {
          start: BytePos(0),
          end: BytePos(3),
          filename: *filename,
        },
      ])
    });
  }
}
