# nix-build default.nix -A p3
# nix-shell default.nix -A p3
# nix-shell default.nix -A post_install
let 
    pkgs = import <nixpkgs> {};
    stdenv = pkgs.stdenv;
    fetchgit = pkgs.fetchgit;
    ocaml = pkgs.ocaml_4_02_1;
    findlib = pkgs.ocamlPackages_4_02_1.findlib;
in rec {

  p3 = stdenv.mkDerivation {
    name = "p3";
  
#    src = fetchgit {
#      url = https://github.com/tomjridge/p3.git;
#      rev = "0e42a29";
#      sha256 = "795b8bacbea102021ad4aaa819d578e58fd7d2041eba60e36482e04e01f81c32";
#    };
    src=./.;
  
    buildInputs = [ ocaml findlib ];
  
    configurePhase = "true"; 	# Skip configure
  
    createFindlibDestdir = true;

  };

  # get a nix-shell environment with p3 installed via ocamlfind
  post_install = stdenv.mkDerivation {
    name = "post_install";
  
    buildInputs = [ ocaml findlib p3 ];

  };

}
