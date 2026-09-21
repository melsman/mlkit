let
	nixpkgs = fetchTarball {
		url = "https://github.com/NixOS/nixpkgs/archive/78e34d1667d32d8a0ffc3eba4591ff256e80576e.tar.gz";
	};
	pkgs = import nixpkgs {};

    # Build MLKit package using the fetched source
	mlkitPinned = pkgs.stdenv.mkDerivation {
	  name = "mlkit-pinned";
	  version = "git-4858b8b";

	  # Fetch MLKit from GitHub at specific commit
	  src = pkgs.fetchFromGitHub {
		owner = "melsman";
		repo = "mlkit";
		rev = "55af1d1921c122d52da8fdcea664e6ca2a495a2d";
		sha256 = "0ihijkdwxjsi6glj1vlf3sji76irsqazw817hcy39h7vl3n18v8f";
	  };
	  
	  nativeBuildInputs = [
		pkgs.mlton
		pkgs.autoreconfHook
	  ];

	  buildFlags = [
		"mlkit"
		"mlkit_libs"
	  ];

	  doCheck = true;

	  # MLKit intentionally has some of these in its test suite.
	  # Since the test suite is available in `$out/share/mlkit/test`, we must disable this check.
	  dontCheckForBrokenSymlinks = true;
	};
in
	pkgs.stdenv.mkDerivation {
		name = "specialeMikkel";

		buildInputs =
			with pkgs;
			[ autoreconfHook
			  cacert
			  curl
			  futhark
			  gcc9
			  ghostscript
			  git
			  gnumake42
			  lima
			  mlkitPinned
			  mlton
			  nettools
			  smlpkg
			  tshark
			  tunctl
			  unzip
			  xen
			];

		shellHook = ''
		  	export NIX_ENFORCE_PURITY=0

			export CC=${pkgs.gcc9}/bin/gcc
			export CXX=${pkgs.gcc9}/bin/g++
			export PATH=${pkgs.gcc9}/bin:$PATH

			fish
		'';
	}
