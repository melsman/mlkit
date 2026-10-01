.PHONY: clean
clean:
	rm -rf MLB run *~ *.vcg *.log
	rm -rf io/MLB io/*~ io/*.vcg io/*.log

MLKIT ?= ../bin/mlkit -j 2

MLKIT_BASIS_VARIANTS = ri par gc gc_rp par_rp ri_rp
MLKIT_BASIS_TARGETS = $(addprefix mlkit_basislib_,$(MLKIT_BASIS_VARIANTS))

.PHONY: mlkit_basislibs mlkit_basislibs_dirs $(MLKIT_BASIS_TARGETS)
mlkit_basislibs: $(MLKIT_BASIS_TARGETS)

# The compiler creates directories with a check followed by mkdir. Create the
# shared parents before starting jobs; each variant has its own subdirectory.
mlkit_basislibs_dirs:
	mkdir -p MLB io/MLB

$(MLKIT_BASIS_TARGETS): | mlkit_basislibs_dirs

mlkit_basislib_ri:
	SML_LIB=.. $(MLKIT) -c -no_gc repl.mlb

mlkit_basislib_par:
	SML_LIB=.. $(MLKIT) -c -no_gc -par repl.mlb

mlkit_basislib_gc:
	SML_LIB=.. $(MLKIT) -c -gc repl.mlb

mlkit_basislib_gc_rp:
	SML_LIB=.. $(MLKIT) -c -gc -region_profile repl.mlb

mlkit_basislib_par_rp:
	SML_LIB=.. $(MLKIT) -c -no_gc -par -region_profile repl.mlb

mlkit_basislib_ri_rp:
	SML_LIB=.. $(MLKIT) -c -no_gc -region_profile -Pcee -print_rho_types -Paux -log_to_file repl.mlb
