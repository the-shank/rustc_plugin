#![feature(rustc_private)]
#![allow(dead_code)]

extern crate rustc_driver;
extern crate rustc_driver_impl;
extern crate rustc_hir;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_type_ir;

use std::{borrow::Cow, collections::HashMap, env, process::Command};

use clap::Parser;
use rustc_hir::{
  def::DefKind, def_id::DefId, Item, ItemKind, Node, QPath, TyKind as HirTyKind,
  VariantData,
};
use rustc_middle::ty::{TyCtxt, TyKind};
use rustc_plugin::{CrateFilter, RustcPlugin, RustcPluginArgs, Utf8Path};
use rustc_span::{def_id::LocalDefId, Span};
use serde::{Deserialize, Serialize};

// This struct is the plugin provided to the rustc_plugin framework,
// and it must be exported for use by the CLI/driver binaries.
pub struct R4EChecker;

// To parse CLI arguments, we use Clap for this example. But that
// detail is up to you.
#[derive(Serialize, Deserialize)]
pub struct R4ECheckerArgs;

pub struct R4ECheckerCallbacks;

impl rustc_driver::Callbacks for R4ECheckerCallbacks {
  fn after_analysis<'tcx>(
    &mut self,
    _handler: &rustc_session::EarlyErrorHandler,
    _compiler: &rustc_interface::interface::Compiler,
    queries: &'tcx rustc_interface::Queries<'tcx>,
  ) -> rustc_driver_impl::Compilation {
    // extract the TyCtxt for analysis
    let mut query_result = queries.global_ctxt().unwrap();
    query_result.enter(|tcx| r4e_analysis(tcx));
    rustc_driver_impl::Compilation::Continue
  }
  // At the top-level, the Rustc API uses an event-based interface for
  // accessing the compiler at different stages of compilation. In this callback,
  // all the type-checking has completed.
}

fn get_typ_and_id_from_res(res: &rustc_hir::def::Res, tcx: TyCtxt) -> (DefKind, DefId) {
  match *res {
    rustc_hir::def::Res::Def(def_kind, def_id) => (def_kind, def_id),
    rustc_hir::def::Res::PrimTy(_) => unimplemented!(),
    rustc_hir::def::Res::SelfTyParam { trait_ } => unimplemented!(),
    rustc_hir::def::Res::SelfTyAlias {
      alias_to,
      forbid_generic,
      is_trait_impl,
    } => unimplemented!(),
    rustc_hir::def::Res::SelfCtor(_) => unimplemented!(),
    rustc_hir::def::Res::Local(_) => unimplemented!(),
    rustc_hir::def::Res::ToolMod => unimplemented!(),
    rustc_hir::def::Res::NonMacroAttr(_) => unimplemented!(),
    rustc_hir::def::Res::Err => unimplemented!(),
  }
}

fn get_typ_and_id_from_kind(kind: &HirTyKind, tcx: TyCtxt) -> (DefKind, DefId) {
  match *kind {
    HirTyKind::Slice(_) => unimplemented!(),
    HirTyKind::Array(_, _) => unimplemented!(),
    HirTyKind::Ptr(_) => unimplemented!(),
    HirTyKind::Ref(_, _) => unimplemented!(),
    HirTyKind::BareFn(_) => unimplemented!(),
    HirTyKind::Never => unimplemented!(),
    HirTyKind::Tup(_) => unimplemented!(),
    HirTyKind::Path(qpath) => match qpath {
      QPath::Resolved(_ty, path) => get_typ_and_id_from_res(&path.res, tcx),
      QPath::TypeRelative(_, _) => unimplemented!(),
      QPath::LangItem(_, _, _) => unimplemented!(),
    },
    HirTyKind::OpaqueDef(_, _, _) => unimplemented!(),
    HirTyKind::TraitObject(_, _, _) => unimplemented!(),
    HirTyKind::Typeof(_) => unimplemented!(),
    HirTyKind::Infer => unimplemented!(),
    HirTyKind::Err(_) => unimplemented!(),
  }
}

fn handle_item(
  item: &Item,
  tcx: TyCtxt,
  results: &mut HashMap<LocalDefId, (ItemInfo, AnalysisResult)>,
) {
  let local_def_id = item.owner_id.def_id;
  match item.kind {
    ItemKind::ExternCrate(_) => {
      println!("item_kind:ExternCrate => skipping");
      // put_result(item_id, item, AnalysisResult::Skipped, results);
    }
    ItemKind::Use(_, _) => {
      println!("item_kind:Use => skipping");
      // put_result(item_id, item, AnalysisResult::Skipped, results);
    }
    ItemKind::Static(_, _, _) => {
      println!("item_kind:Static");
      unimplemented!("todo");
    }
    ItemKind::Const(_, _, _) => {
      println!("item_kind:Const");
      unimplemented!("todo")
    }
    ItemKind::Fn(fn_sig, generics, _body_id) => {
      println!("item_kind:Fn");
      println!("num_generic_params: {}", generics.params.len());
      println!("num_args: {}", fn_sig.decl.inputs.len());
      if !generics.params.is_empty() {
        println!("has generic params => NOT SUPPORTED");
        put_result(local_def_id, item, AnalysisResult::NotSupported, results);
        // return AnalysisResult::NotSupported;
        return;
      } else {
        for (i, input) in fn_sig.decl.inputs.iter().enumerate() {
          println!("arg {i}:");
          match handle_ty_kind(&input.kind, tcx) {
            Ok(_) => {}
            Err(cause) => {
              println!("{cause} => NOT SUPPORTED");
              put_result(local_def_id, item, AnalysisResult::NotSupported, results);
              // return AnalysisResult::NotSupported;
              return;
            }
          }
        }
      }
      println!("ALL GOOD");
      put_result(local_def_id, item, AnalysisResult::Ok, results);
      // AnalysisResult::Ok
    }
    ItemKind::Macro(_, _) => {
      println!("item_kind:Macro => skipping");
      // put_result(item_id, item, AnalysisResult::Skipped, results);
    }
    ItemKind::Mod(_) => {
      println!("item_kind:Mod => skipping");
      // put_result(item_id, item, AnalysisResult::Skipped, results);
    }
    ItemKind::ForeignMod { abi: _, items: _ } => {
      println!("item_kind:ForeignMod => skipping");
      // put_result(item_id, item, AnalysisResult::Skipped, results);
    }
    ItemKind::GlobalAsm(_) => {
      println!("item_kind:GlobalAsm");
      unimplemented!("todo")
    }
    ItemKind::TyAlias(_, _) => {
      println!("item_kind:TyAlias");
      unimplemented!("todo")
    }
    ItemKind::OpaqueTy(_) => {
      println!("item_kind:OpaqueTy");
      unimplemented!("todo")
    }
    ItemKind::Enum(_, _) => {
      println!("item_kind:Enum");
      unimplemented!("todo")
    }
    ItemKind::Struct(variant_data, generics) => {
      println!("item_kind:Struct");
      println!("num_generic_params: {}", generics.params.len());
      println!("num_fields: {}", variant_data.fields().len());
      if !generics.params.is_empty() {
        println!("has generic params => NOT SUPPORTED");
        put_result(local_def_id, item, AnalysisResult::NotSupported, results);
        // return AnalysisResult::NotSupported;
        return;
      } else {
        match variant_data {
          VariantData::Struct(field_defs, _) | VariantData::Tuple(field_defs, _, _) => {
            for (i, field) in field_defs.iter().enumerate() {
              println!("field {i}:");
              if let Err(cause) = handle_ty_kind(&field.ty.kind, tcx) {
                println!("{cause} => NOT SUPPORTED");
                put_result(local_def_id, item, AnalysisResult::NotSupported, results);
                // return AnalysisResult::NotSupported;
                return;
              }
            }
          }
          VariantData::Unit(_, _) => {
            println!("is unit supported?");
            unimplemented!("todo")
          }
        }
      };
      println!("ALL GOOD");
      put_result(local_def_id, item, AnalysisResult::Ok, results);
      // AnalysisResult::Ok
    }
    ItemKind::Union(_, _) => {
      println!("item_kind:Union");
      unimplemented!("todo")
    }
    ItemKind::Trait(_, _, _, _, _) => {
      println!("item_kind:Trait => skipping");
      // put_result(item_id, item, AnalysisResult::Skipped, results);
    }
    ItemKind::TraitAlias(_, _) => {
      println!("item_kind:TraitAlias => skipping");
      // put_result(item_id, item, AnalysisResult::Skipped, results);
    }
    ItemKind::Impl(impl_inst) => {
      println!("item_kind:Impl");
      // println!("item.kind: {:#?}", item.kind);
      // get the type for which this trait is being implemented
      // updated that type to be not-supported
      // impl_inst.self_ty.kind

      let self_ty_kind = impl_inst.self_ty.kind;
      let (ty_typ, ty_def_id) = get_typ_and_id_from_kind(&self_ty_kind, tcx);
      if let Some(ty_local_def_id) = ty_def_id.as_local() {
        if let DefKind::Struct = ty_typ {
          let ty_node = tcx.hir().get_by_def_id(ty_local_def_id);
          if let Node::Item(ty_item) = ty_node {
            put_result(
              ty_local_def_id,
              ty_item,
              AnalysisResult::NotSupported,
              results,
            )
          } else {
            println!("[!] node associated with ty_local_def_id is not and Item!");
            println!("ty_local_def_id: {ty_local_def_id:#?}");
            println!("ty_node: {ty_node:#?}");
            panic!("see above messages");
          }
        }
      } else {
        println!("non local ty_def_id for ty_typ:{ty_typ:?}");
        // do nothing
      }
    }
  }
}

fn handle_res<'b>(res: &rustc_hir::def::Res, tcx: TyCtxt) -> Result<(), &'b str> {
  match res {
    rustc_hir::def::Res::Def(_, def_id) => {
      println!("res:Def");
      let ty_kind = tcx.type_of(def_id).skip_binder().kind();
      handle_ty_ty_kind(ty_kind, tcx)
    }
    rustc_hir::def::Res::PrimTy(_) => {
      println!("res:PrimTy");
      Ok(())
    }
    rustc_hir::def::Res::SelfTyParam { trait_ } => {
      println!("res:SelfTyParam");
      unimplemented!("todo")
    }
    rustc_hir::def::Res::SelfTyAlias {
      alias_to,
      forbid_generic,
      is_trait_impl,
    } => {
      println!("res:SelfTyAlias");
      unimplemented!("todo")
    }
    rustc_hir::def::Res::SelfCtor(_) => {
      println!("res:SelfCtor");
      unimplemented!("todo")
    }
    rustc_hir::def::Res::Local(_) => {
      println!("res:Local");
      unimplemented!("todo")
    }
    rustc_hir::def::Res::ToolMod => {
      println!("res:TooolMod");
      unimplemented!("todo")
    }
    rustc_hir::def::Res::NonMacroAttr(_) => {
      println!("res:NonMacroAttr");
      unimplemented!("todo")
    }
    rustc_hir::def::Res::Err => {
      println!("res:Err");
      unimplemented!("todo")
    }
  }
}

fn handle_path<'b>(path: &rustc_hir::Path, tcx: TyCtxt) -> Result<(), &'b str> {
  handle_res(&path.res, tcx)
}

fn handle_qpath<'tcx, 'b>(qpath: &'tcx QPath<'tcx>, tcx: TyCtxt) -> Result<(), &'b str> {
  match *qpath {
    QPath::Resolved(_, path) => {
      println!("qpath:Resolved");
      // println!("qpath = {qpath:#?}");
      handle_res(&path.res, tcx)
    }
    QPath::TypeRelative(_, _) => {
      println!("qpath:TypeRelative (GENERICS)");
      unimplemented!("todo")
    }
    QPath::LangItem(_, _, _) => {
      println!("qpath:LangItem (NOT SUPPORTED)");
      unimplemented!("todo")
    }
  }
}

fn handle_ty_ty_kind<'b>(ty_kind: &TyKind, tcx: TyCtxt) -> Result<(), &'b str> {
  match *ty_kind {
    TyKind::Bool => {
      println!("ty_ty_kind:Bool");
      Ok(())
    }
    TyKind::Char => {
      println!("ty_ty_kind:Char");
      Ok(())
    }
    TyKind::Int(_) => {
      println!("ty_ty_kind:Int");
      Ok(())
    }
    TyKind::Uint(_) => {
      println!("ty_ty_kind:Uint");
      Ok(())
    }
    TyKind::Float(_) => {
      println!("ty_ty_kind:Float");
      Ok(())
    }
    TyKind::Adt(adt_def, generic_args_ref) => {
      println!("ty_ty_kind:Adt");
      println!("num_generic_args: {}", generic_args_ref.len());
      if generic_args_ref.len() > 0 {
        return Err("has generic params");
      }
      if adt_def.is_struct() {
        let vdef = adt_def.variants().iter().next().unwrap();
        for (i, field) in vdef.fields.iter().enumerate() {
          println!("field {i}:");
          let field_def_id = field.did;
          let field_ty_kind = tcx.type_of(field_def_id).skip_binder().kind();
          if let Err(cause) = handle_ty_ty_kind(field_ty_kind, tcx) {
            println!("{cause} => NOT SUPPORTED");
            return Err(cause);
          }
        }
        Ok(())
      } else {
        unimplemented!("todo")
      }
    }
    TyKind::Foreign(_) => {
      println!("ty_ty_kind:Foreign");
      unimplemented!("todo")
    }
    TyKind::Str => {
      println!("ty_ty_kind:Str");
      Ok(())
    }
    TyKind::Array(_, _) => {
      println!("ty_ty_kind:Array");
      unimplemented!("todo")
    }
    TyKind::Slice(_) => {
      println!("ty_ty_kind:Slice");
      unimplemented!("todo")
    }
    TyKind::RawPtr(_) => {
      println!("ty_ty_kind:RawPtr");
      unimplemented!("todo")
    }
    TyKind::Ref(_, _, _) => {
      println!("ty_ty_kind:Ref");
      unimplemented!("todo")
    }
    TyKind::FnDef(_, _) => {
      println!("ty_ty_kind:FnDef");
      unimplemented!("todo")
    }
    TyKind::FnPtr(_) => {
      println!("ty_ty_kind:FnPtr");
      unimplemented!("todo")
    }
    TyKind::Dynamic(_, _, _) => {
      println!("ty_ty_kind:Dynamic");
      unimplemented!("todo")
    }
    TyKind::Closure(_, _) => {
      println!("ty_ty_kind:Closure");
      unimplemented!("todo")
    }
    TyKind::Generator(_, _, _) => {
      println!("ty_ty_kind:Generator");
      unimplemented!("todo")
    }
    TyKind::GeneratorWitness(_) => {
      println!("ty_ty_kind:GeneratorWitness");
      unimplemented!("todo")
    }
    TyKind::GeneratorWitnessMIR(_, _) => {
      println!("ty_ty_kind:GeneratorWitnessMIR");
      unimplemented!("todo")
    }
    TyKind::Never => {
      println!("ty_ty_kind:Never");
      unimplemented!("todo")
    }
    TyKind::Tuple(_) => {
      println!("ty_ty_kind:Tuple");
      unimplemented!("todo")
    }
    TyKind::Alias(_, _) => {
      println!("ty_ty_kind:Alias");
      unimplemented!("todo")
    }
    TyKind::Param(_) => {
      println!("ty_ty_kind:Param");
      unimplemented!("todo")
    }
    TyKind::Bound(_, _) => {
      println!("ty_ty_kind:Bound");
      unimplemented!("todo")
    }
    TyKind::Placeholder(_) => {
      println!("ty_ty_kind:Placeholder");
      unimplemented!("todo")
    }
    TyKind::Infer(_) => {
      println!("ty_ty_kind:Infer");
      unimplemented!("todo")
    }
    TyKind::Error(_) => {
      println!("ty_ty_kind:Error");
      unimplemented!("todo")
    }
  }
}

fn handle_ty_kind<'tcx, 'b>(
  ty_kind: &'tcx HirTyKind<'tcx>,
  tcx: TyCtxt,
) -> Result<(), &'b str> {
  match *ty_kind {
    HirTyKind::Slice(_) => {
      println!("ty_kind:Slice");
      unimplemented!("todo")
    }
    HirTyKind::Array(_, _) => {
      println!("ty_kind:Array");
      unimplemented!("todo")
    }
    HirTyKind::Ptr(_) => {
      println!("ty_kind:Ptr");
      unimplemented!("todo")
    }
    HirTyKind::Ref(_, _) => {
      println!("ty_kind:Ref");
      unimplemented!("todo")
    }
    HirTyKind::BareFn(_) => {
      println!("ty_kind:BareFn");
      unimplemented!("todo")
    }
    HirTyKind::Never => {
      println!("ty_kind:Never");
      unimplemented!("todo")
    }
    HirTyKind::Tup(_) => {
      println!("ty_kind:Tup");
      unimplemented!("todo")
    }
    HirTyKind::Path(qpath) => {
      println!("ty_kind:Path");
      handle_qpath(&qpath, tcx)
    }
    HirTyKind::OpaqueDef(_, _, _) => {
      println!("ty_kind:OpaqueDef");
      unimplemented!("todo")
    }
    HirTyKind::TraitObject(_, _, _) => {
      println!("ty_kind:TraitObject");
      unimplemented!("todo")
    }
    HirTyKind::Typeof(_) => {
      println!("ty_kind:TypeOf");
      unimplemented!("todo")
    }
    HirTyKind::Infer => {
      println!("ty_kind:Infer");
      unimplemented!("todo")
    }
    HirTyKind::Err(_) => {
      println!("ty_kind:Err");
      unimplemented!("todo")
    }
  }
}

// what we need as result:
// - id (key for the map)
// - name
// - item type (struct / function)
// - compatible or not?
// - span (location)

/// final analysis result
#[derive(Debug)]
enum AnalysisResult {
  NotSupported,
  Ok,
  Skipped,
}

// TODO: should we add more?
#[derive(Debug, Copy, Clone)]
enum RelevantType {
  Struct,
  Fn,
}

#[derive(Debug, Clone)]
struct ItemInfo {
  name: String,
  item_type: RelevantType,
  span: Span,
}

fn create_item_info(item: &Item) -> ItemInfo {
  let name = item.ident.name.to_string();
  match item.kind {
    ItemKind::Struct(_, _) => {
      let item_type = RelevantType::Struct;
      ItemInfo {
        name,
        item_type,
        span: item.span,
      }
    }
    ItemKind::Fn(_, _, _) => {
      let item_type = RelevantType::Fn;
      ItemInfo {
        name,
        item_type,
        span: item.span,
      }
    }
    _ => {
      panic!("call this only for Struct/Fn");
    }
  }
}

fn put_result(
  local_def_id: LocalDefId,
  item: &Item,
  result: AnalysisResult,
  result_map: &mut HashMap<LocalDefId, (ItemInfo, AnalysisResult)>,
) {
  match result_map.get(&local_def_id) {
    Some((_, AnalysisResult::NotSupported)) | Some((_, AnalysisResult::Skipped)) => {
      // do nothing
    }
    Some((item_info, AnalysisResult::Ok)) => {
      if let AnalysisResult::NotSupported = result {
        result_map.insert(local_def_id, (item_info.clone(), result));
      }
    }
    None => {
      let item_info = create_item_info(item);
      result_map.insert(local_def_id, (item_info, result));
    }
  };
  println!("put_result: result_map: {:#?}", result_map);
}

fn hirs(tcx: TyCtxt) {
  println!(">> hirs...");
  println!("==========");
  let hir = tcx.hir();
  let mut analysis_results: HashMap<LocalDefId, (ItemInfo, AnalysisResult)> =
    HashMap::new();
  for item_id in hir.items() {
    let item = hir.item(item_id);
    let name = item.ident.name;
    println!("name:{name}");
    handle_item(item, tcx, &mut analysis_results);

    println!("==========");
  }
}

fn mirs(tcx: TyCtxt) {
  println!(">> mirs...");
  println!("==========");
  let fn_ids = tcx.mir_keys(());
  for fn_id in fn_ids {
    let def_id = fn_id.to_def_id();
    let mir = tcx.optimized_mir(def_id);
    println!("{:#?}", def_id);
    println!("{:#?}", mir);

    for arg in mir.args_iter() {}
    println!("==========");
  }
}

// core of the analysis.
// WIP
fn r4e_analysis(tcx: TyCtxt) {
  println!(">> in r4e_analysis");
  hirs(tcx);
  // mirs(tcx);
}

impl RustcPlugin for R4EChecker {
  type Args = R4ECheckerArgs;

  fn version(&self) -> Cow<'static, str> {
    env!("CARGO_PKG_VERSION").into()
  }

  fn driver_name(&self) -> Cow<'static, str> {
    "r4e-analysis-driver".into()
  }

  // In the CLI, we ask Clap to parse arguments and also specify a CrateFilter.
  // If one of the CLI arguments was a specific file to analyze, then you
  // could provide a different filter.
  fn args(&self, _target_dir: &Utf8Path) -> rustc_plugin::RustcPluginArgs<Self::Args> {
    let filter = CrateFilter::OnlyWorkspace;
    RustcPluginArgs {
      args: R4ECheckerArgs,
      filter,
    }
  }

  // In the driver, we use the Rustc API to start a compiler session
  // for the arguments given to us by rustc_plugin.
  fn run(
    self,
    compiler_args: Vec<String>,
    _plugin_args: Self::Args,
  ) -> rustc_interface::interface::Result<()> {
    let mut callbacks = R4ECheckerCallbacks;
    let compiler = rustc_driver::RunCompiler::new(&compiler_args, &mut callbacks);
    compiler.run()
  }
}

// ===================================================
// Existing code
// ===================================================

// This struct is the plugin provided to the rustc_plugin framework,
// and it must be exported for use by the CLI/driver binaries.
pub struct PrintAllItemsPlugin;

// To parse CLI arguments, we use Clap for this example. But that
// detail is up to you.
#[derive(Parser, Serialize, Deserialize)]
pub struct PrintAllItemsPluginArgs {
  #[arg(short, long)]
  allcaps: bool,

  #[clap(last = true)]
  cargo_args: Vec<String>,
}

impl RustcPlugin for PrintAllItemsPlugin {
  type Args = PrintAllItemsPluginArgs;

  fn version(&self) -> Cow<'static, str> {
    env!("CARGO_PKG_VERSION").into()
  }

  fn driver_name(&self) -> Cow<'static, str> {
    "print-all-items-driver".into()
  }

  // In the CLI, we ask Clap to parse arguments and also specify a CrateFilter.
  // If one of the CLI arguments was a specific file to analyze, then you
  // could provide a different filter.
  fn args(&self, _target_dir: &Utf8Path) -> RustcPluginArgs<Self::Args> {
    let args = PrintAllItemsPluginArgs::parse_from(env::args().skip(1));
    let filter = CrateFilter::AllCrates;
    RustcPluginArgs { args, filter }
  }

  // Pass Cargo arguments (like --feature) from the top-level CLI to Cargo.
  fn modify_cargo(&self, cargo: &mut Command, args: &Self::Args) {
    cargo.args(&args.cargo_args);
  }

  // In the driver, we use the Rustc API to start a compiler session
  // for the arguments given to us by rustc_plugin.
  fn run(
    self,
    compiler_args: Vec<String>,
    plugin_args: Self::Args,
  ) -> rustc_interface::interface::Result<()> {
    let mut callbacks = PrintAllItemsCallbacks { args: plugin_args };
    let compiler = rustc_driver::RunCompiler::new(&compiler_args, &mut callbacks);
    compiler.run()
  }
}

struct PrintAllItemsCallbacks {
  args: PrintAllItemsPluginArgs,
}

impl rustc_driver::Callbacks for PrintAllItemsCallbacks {
  // At the top-level, the Rustc API uses an event-based interface for
  // accessing the compiler at different stages of compilation. In this callback,
  // all the type-checking has completed.
  fn after_analysis<'tcx>(
    &mut self,
    _handler: &rustc_session::EarlyErrorHandler,
    _compiler: &rustc_interface::interface::Compiler,
    queries: &'tcx rustc_interface::Queries<'tcx>,
  ) -> rustc_driver::Compilation {
    // We extract a key data structure, the `TyCtxt`, which is all we need
    // for our simple task of printing out item names.
    queries
      .global_ctxt()
      .unwrap()
      .enter(|tcx| print_all_items(tcx, &self.args));

    // Note that you should generally allow compilation to continue. If
    // your plugin is being invoked on a dependency, then you need to ensure
    // the dependency is type-checked (its .rmeta file is emitted into target/)
    // so that its dependents can read the compiler outputs.
    rustc_driver::Compilation::Continue
  }
}

// The core of our analysis. It doesn't do much, just access some methods on the `TyCtxt`.
// I recommend reading the Rustc Development Guide to better understand which compiler APIs
// are relevant to whatever task you have.
fn print_all_items(tcx: TyCtxt, args: &PrintAllItemsPluginArgs) {
  let hir = tcx.hir();
  for item_id in hir.items() {
    let item = hir.item(item_id);
    let mut msg = format!(
      "There is an item \"{}\" of type \"{}\"",
      item.ident,
      item.kind.descr()
    );
    if args.allcaps {
      msg = msg.to_uppercase();
    }
    println!("{msg}");
  }
}
