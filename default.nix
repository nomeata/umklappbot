{ sources ? import ./nix/sources.nix }:     # import the sources
with
  { overlay = _: pkgs:
      { niv = import sources.niv {};    # use the sources :)
      };
  };
let pkgs = (import (sources.nixpkgs-static + "/survey/default.nix") {}).pkgs; in

let compiler = "ghc865"; in
let strip = true; in

let
  telegram-api-pkg = { mkDerivation, stdenv, fetchFromGitHub
	    , aeson
            , containers
            , http-api-data
            , http-client
            , servant
            , servant-client
            , servant-client-core
            , mtl
            , text
            , transformers
            , http-media
            , http-types
            , mime-types
            , string-conversions
            , binary
            , ansi-wl-pprint
            , hjpath
            , hspec
            , http-client-tls
            , optparse-applicative
  }:
      mkDerivation {
        pname = "telegram-api";
        version = "0.7.1.0";
        src = fetchFromGitHub {
          owner = "klappvisor";
          repo = "haskell-telegram-api";
          rev = "abbfd76c40f2783c113b660184a03cc94d58e751";
          sha256 = "0mzhigdyj5jdwycmz587s05zp5c7wcf7njw3x866iln59kp0rgi3";
        };
        isLibrary = true;
        isExecutable = false;
        enableSharedExecutables = false;
        enableSharedLibraries = false;
        libraryHaskellDepends = [
	    aeson
            containers
            http-api-data
            http-client
            servant
            servant-client
            servant-client-core
            mtl
            text
            transformers
            http-media
            http-types
            mime-types
            string-conversions
            binary
            ansi-wl-pprint
            hjpath
            hspec
            http-client-tls
            optparse-applicative
            servant-client
	];
        jailbreak = true;  # want servant-client-0.16.0.1, not 0.16
        license = stdenv.lib.licenses.bsd3;
      };

  umklappbot-pkg = { mkDerivation, base, stdenv,
    aeson,
    aws-lambda-haskell-runtime,
    telegram-api,
    aws,
    pretty-show,
  }:
      mkDerivation {
        pname = "umklappbot";
        version = "0.1.0.0";
        src = pkgs.lib.sourceByRegex ./. [
          ".*\.cabal$"
          "LICENSE"
          "^src.*"
        ];
        isLibrary = false;
        isExecutable = true;
        enableSharedExecutables = false;
        enableSharedLibraries = false;
        executableHaskellDepends = [ base
            aeson
	    aws-lambda-haskell-runtime
	    telegram-api
	    aws
            pretty-show
	];
        license = stdenv.lib.licenses.bsd3;
        configureFlags = [
          "--ghc-option=-optl=-static"
          "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
          "--extra-lib-dirs=${pkgs.zlib.static}/lib"
          "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
        ] ++ pkgs.lib.optionals (!strip) [
          "--disable-executable-stripping"
        ] ;
      };

  haskellPackages = with pkgs.haskell.lib; pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      # Dependencies we need to patch
      hpc-coveralls = appendPatch super.hpc-coveralls (builtins.fetchurl https://github.com/guillaume-nargeot/hpc-coveralls/pull/73/commits/344217f513b7adfb9037f73026f5d928be98d07f.patch);
      telegram-api = self.callPackage telegram-api-pkg {};
    };
  };

  umklappbot = haskellPackages.callPackage umklappbot-pkg {};

  function-zip = pkgs.runCommandNoCC "umklappbot-function.zip" {
    buildInputs = [ pkgs.zip ];
  } ''
    mkdir -p $out
    cp ${umklappbot}/bin/umklappbot bootstrap
    zip $out/function.zip bootstrap
  '';

  shell = umklappbot.env.overrideAttrs(old: {
    preferLocalBuild = true;
    allowSubstitutes = true;
  });

in
  { inherit
      umklappbot
      function-zip
      shell;
  }
