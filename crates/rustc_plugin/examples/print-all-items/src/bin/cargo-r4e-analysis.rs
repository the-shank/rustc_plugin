fn main() {
  env_logger::init();
  // rustc_plugin::cli_main(print_all_items::PrintAllItemsPlugin);
  rustc_plugin::cli_main(print_all_items::R4EChecker);
}
