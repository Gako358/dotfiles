{
  c = {
    description = "C environment";
    path = ./c;
  };

  tex = {
    description = "TeX environment";
    path = ./latex;
  };

  python = {
    description = "Python environment";
    path = ./python;
  };

  rust = {
    description = "Rust environment";
    path = ./rust;
  };

  dev = {
    description = "Development environment";
    path = ./devshell;
  };

  wasm = {
    description = "WebAssembly environment";
    path = ./wasm;
  };

  java = {
    description = "Java environment";
    path = ./java;
  };

  nodejs = {
    description = "Node.js environment";
    path = ./nodejs;
  };
}
