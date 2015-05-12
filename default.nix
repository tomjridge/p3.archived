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

  # following needs 4.01.0 because of string literal problem
  p3_2014-04-30 = stdenv.mkDerivation {
    name = "p3";
  
    src = fetchgit {
      url = https://github.com/tomjridge/p3.git;
      rev = "1ee8caf"; # 2014-04-30
      sha256 = "6e78de60e45b1ab72d4892f24345a921e546774d4ed3a1578d1bac73494c5394";
    };
  
    buildInputs = [ pkgs.ocaml_4_01_0 pkgs.ocamlPackages.findlib ]; # note use of 4.01.0
  
    configurePhase = "true"; 	# Skip configure

    postBuild = '' 
      mkdir -p $out
      cp -R -L build examples $out # want symlinks to point in $out, not at their original targets
    '';
    
    installPhase= "true"; # skip install  

#    createFindlibDestdir = true;


  };


  # get a nix-shell environment with p3 installed via ocamlfind
  post_install = stdenv.mkDerivation {
    name = "post_install";
  
    buildInputs = [ ocaml findlib p3 ];

  };

}
