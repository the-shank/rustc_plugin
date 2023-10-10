#![feature(rustc_private)]
#![allow(dead_code)]

extern crate rustc_driver;
extern crate rustc_driver_impl;
extern crate rustc_hir;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_session;
extern crate rustc_type_ir;

use std::{borrow::Cow, env, process::Command};

use clap::Parser;
use rustc_hir::{ItemKind, QPath, TyKind as HirTyKind, VariantData};
use rustc_middle::ty::{TyCtxt, TyKind};
use rustc_plugin::{CrateFilter, RustcPlugin, RustcPluginArgs, Utf8Path};
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

fn handle_item_kind(item_kind: &ItemKind, tcx: TyCtxt) {
  match *item_kind {
    ItemKind::ExternCrate(_) => {
      println!("item_kind:ExternCrate => skipping");
    }
    ItemKind::Use(_, _) => {
      println!("item_kind:Use => skipping");
    }
    ItemKind::Static(_, _, _) => {
      println!("item_kind:Static");
      todo!("todo");
    }
    ItemKind::Const(_, _, _) => {
      println!("item_kind:Const");
      todo!("todo")
    }
    ItemKind::Fn(fn_sig, generics, _body_id) => {
      println!("item_kind:Fn");
      println!("num_generic_params: {}", generics.params.len());
      println!("num_args: {}", fn_sig.decl.inputs.len());
      if !generics.params.is_empty() {
        println!("has generic params => NOT SUPPORTED");
        return;
      } else {
        for (i, input) in fn_sig.decl.inputs.iter().enumerate() {
          println!("arg {i}:");
          match handle_ty_kind(&input.kind, tcx) {
            Ok(_) => {}
            Err(cause) => {
              println!("{cause} => NOT SUPPORTED");
              return;
            }
          }
        }
      }
      println!("ALL GOOD");
    }
    ItemKind::Macro(_, _) => {
      println!("item_kind:Macro => skipping");
    }
    ItemKind::Mod(_) => {
      println!("item_kind:Mod => skipping");
    }
    ItemKind::ForeignMod { abi: _, items: _ } => {
      println!("item_kind:ForeignMod => skipping");
    }
    ItemKind::GlobalAsm(_) => {
      println!("item_kind:GlobalAsm");
      todo!("todo")
    }
    ItemKind::TyAlias(_, _) => {
      println!("item_kind:TyAlias");
      todo!("todo")
    }
    ItemKind::OpaqueTy(_) => {
      println!("item_kind:OpaqueTy");
      todo!("todo")
    }
    ItemKind::Enum(_, _) => {
      println!("item_kind:Enum");
      todo!("todo")
    }
    ItemKind::Struct(variant_data, generics) => {
      println!("item_kind:Struct");
      println!("num_generic_params: {}", generics.params.len());
      println!("num_fields: {}", variant_data.fields().len());
      if !generics.params.is_empty() {
        println!("has generic params => NOT SUPPORTED");
        return;
      } else {
        match variant_data {
          VariantData::Struct(field_defs, _) | VariantData::Tuple(field_defs, _, _) => {
            for (i, field) in field_defs.iter().enumerate() {
              println!("field {i}:");
              if let Err(cause) = handle_ty_kind(&field.ty.kind, tcx) {
                println!("{cause} => NOT SUPPORTED");
                return;
              }
            }
          }
          VariantData::Unit(_, _) => {
            println!("is unit supported?");
            todo!("todo")
          }
        }
      };
      println!("ALL GOOD");
    }
    ItemKind::Union(_, _) => {
      println!("item_kind:Union");
      todo!("todo")
    }
    ItemKind::Trait(_, _, _, _, _) => {
      println!("item_kind:Trait");
      todo!("todo")
    }
    ItemKind::TraitAlias(_, _) => {
      println!("item_kind:TraitAlias");
      todo!("todo")
    }
    ItemKind::Impl(_) => {
      println!("item_kind:Impl");
      todo!("todo")
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
      todo!("todo")
    }
    rustc_hir::def::Res::SelfTyAlias {
      alias_to,
      forbid_generic,
      is_trait_impl,
    } => {
      println!("res:SelfTyAlias");
      todo!("todo")
    }
    rustc_hir::def::Res::SelfCtor(_) => {
      println!("res:SelfCtor");
      todo!("todo")
    }
    rustc_hir::def::Res::Local(_) => {
      println!("res:Local");
      todo!("todo")
    }
    rustc_hir::def::Res::ToolMod => {
      println!("res:TooolMod");
      todo!("todo")
    }
    rustc_hir::def::Res::NonMacroAttr(_) => {
      println!("res:NonMacroAttr");
      todo!("todo")
    }
    rustc_hir::def::Res::Err => {
      println!("res:Err");
      todo!("todo")
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
      todo!("todo")
    }
    QPath::LangItem(_, _, _) => {
      println!("qpath:LangItem (NOT SUPPORTED)");
      todo!("todo")
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
        todo!("todo")
      }
    }
    TyKind::Foreign(_) => {
      println!("ty_ty_kind:Foreign");
      todo!("todo")
    }
    TyKind::Str => {
      println!("ty_ty_kind:Str");
      Ok(())
    }
    TyKind::Array(_, _) => {
      println!("ty_ty_kind:Array");
      todo!("todo")
    }
    TyKind::Slice(_) => {
      println!("ty_ty_kind:Slice");
      todo!("todo")
    }
    TyKind::RawPtr(_) => {
      println!("ty_ty_kind:RawPtr");
      todo!("todo")
    }
    TyKind::Ref(_, _, _) => {
      println!("ty_ty_kind:Ref");
      todo!("todo")
    }
    TyKind::FnDef(_, _) => {
      println!("ty_ty_kind:FnDef");
      todo!("todo")
    }
    TyKind::FnPtr(_) => {
      println!("ty_ty_kind:FnPtr");
      todo!("todo")
    }
    TyKind::Dynamic(_, _, _) => {
      println!("ty_ty_kind:Dynamic");
      todo!("todo")
    }
    TyKind::Closure(_, _) => {
      println!("ty_ty_kind:Closure");
      todo!("todo")
    }
    TyKind::Generator(_, _, _) => {
      println!("ty_ty_kind:Generator");
      todo!("todo")
    }
    TyKind::GeneratorWitness(_) => {
      println!("ty_ty_kind:GeneratorWitness");
      todo!("todo")
    }
    TyKind::GeneratorWitnessMIR(_, _) => {
      println!("ty_ty_kind:GeneratorWitnessMIR");
      todo!("todo")
    }
    TyKind::Never => {
      println!("ty_ty_kind:Never");
      todo!("todo")
    }
    TyKind::Tuple(_) => {
      println!("ty_ty_kind:Tuple");
      todo!("todo")
    }
    TyKind::Alias(_, _) => {
      println!("ty_ty_kind:Alias");
      todo!("todo")
    }
    TyKind::Param(_) => {
      println!("ty_ty_kind:Param");
      todo!("todo")
    }
    TyKind::Bound(_, _) => {
      println!("ty_ty_kind:Bound");
      todo!("todo")
    }
    TyKind::Placeholder(_) => {
      println!("ty_ty_kind:Placeholder");
      todo!("todo")
    }
    TyKind::Infer(_) => {
      println!("ty_ty_kind:Infer");
      todo!("todo")
    }
    TyKind::Error(_) => {
      println!("ty_ty_kind:Error");
      todo!("todo")
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
      todo!("todo")
    }
    HirTyKind::Array(_, _) => {
      println!("ty_kind:Array");
      todo!("todo")
    }
    HirTyKind::Ptr(_) => {
      println!("ty_kind:Ptr");
      todo!("todo")
    }
    HirTyKind::Ref(_, _) => {
      println!("ty_kind:Ref");
      todo!("todo")
    }
    HirTyKind::BareFn(_) => {
      println!("ty_kind:BareFn");
      todo!("todo")
    }
    HirTyKind::Never => {
      println!("ty_kind:Never");
      todo!("todo")
    }
    HirTyKind::Tup(_) => {
      println!("ty_kind:Tup");
      todo!("todo")
    }
    HirTyKind::Path(qpath) => {
      println!("ty_kind:Path");
      handle_qpath(&qpath, tcx)
    }
    HirTyKind::OpaqueDef(_, _, _) => {
      println!("ty_kind:OpaqueDef");
      todo!("todo")
    }
    HirTyKind::TraitObject(_, _, _) => {
      println!("ty_kind:TraitObject");
      todo!("todo")
    }
    HirTyKind::Typeof(_) => {
      println!("ty_kind:TypeOf");
      todo!("todo")
    }
    HirTyKind::Infer => {
      println!("ty_kind:Infer");
      todo!("todo")
    }
    HirTyKind::Err(_) => {
      println!("ty_kind:Err");
      todo!("todo")
    }
  }
}

fn hirs(tcx: TyCtxt) {
  println!(">> hirs...");
  println!("==========");
  let hir = tcx.hir();
  for item_id in hir.items() {
    let item = hir.item(item_id);
    let name = item.ident.name;
    println!("name:{name}");
    handle_item_kind(&item.kind, tcx);

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
