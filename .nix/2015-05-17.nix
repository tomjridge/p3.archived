{ }:
let 
    pkgs = import <nixpkgs> {};
    stdenv = pkgs.stdenv;
    fetchgit = pkgs.fetchgit;
    ocaml = pkgs.ocaml_4_02_1;
    findlib = pkgs.ocamlPackages_4_02_1.findlib;
in
let 
    e3 = stdenv.mkDerivation {
      name = "e3";
    
      src = fetchgit {
        url = https://github.com/tomjridge/e3.git;
        rev = "a5d95f";
        sha256 = "c2985d1b61fe3618d16bc9cea124319e657f5faf92dc8523ec782495bad64723";
      };
    
      buildInputs = [ ocaml findlib ];
    
      configurePhase = "true"; 	# Skip configure
  
      postInstall="cp -R build src $out";
           
      createFindlibDestdir = true;
};
in stdenv.mkDerivation {
    name = "p3";
  
    src = fetchgit {
      url = https://github.com/tomjridge/p3.git;
      rev = "0f6c732";
      sha256 = "5ea4a2486876b2bd382dcdada57593233ac6b1e681539e72d5e144e630aaf1ff";
    };
  
    buildInputs = [ ocaml findlib e3 ];
  
    configurePhase = "true"; 	# Skip configure
  
    postInstall = "mkdir -p $out && cp -R * $out"; # so we can inspect the result

    createFindlibDestdir = true;

}
