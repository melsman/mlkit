.PHONY: clean
clean:
	rm -rf MLB run *~ *.vcg *.log
	rm -rf io/MLB io/*~ io/*.vcg io/*.log

MLKIT ?= ../bin/mlkit -j 2
REML ?= ../bin/reml -j 2

MLKIT_BASIS_VARIANTS = ri par gc gc_rp par_rp ri_rp
PROFILE_LOG_FLAGS = -log_to_file -Prfg -Ppp -Pcee
MLKIT_BASIS_TARGETS = $(addprefix mlkit_basislib_,$(MLKIT_BASIS_VARIANTS))
MLKIT_KIT_TARGETS = $(addprefix mlkit_kitlib_,$(MLKIT_BASIS_VARIANTS))

.PHONY: mlkit_basislibs mlkit_kitlibs mlkit_basislibs_dirs $(MLKIT_BASIS_TARGETS) $(MLKIT_KIT_TARGETS)
mlkit_basislibs: $(MLKIT_BASIS_TARGETS)
mlkit_kitlibs: $(MLKIT_KIT_TARGETS)

# Parallelism is between caches. Within each cache, build the Basis, profiler
# API, and ReML support in order, then the optional full Kit library.
mlkit_basislib_ri mlkit_kitlib_ri: VARIANT_FLAGS = -no_gc
mlkit_basislib_par mlkit_kitlib_par: VARIANT_FLAGS = -no_gc -par
mlkit_basislib_gc mlkit_kitlib_gc: VARIANT_FLAGS = -gc
mlkit_basislib_gc_rp mlkit_kitlib_gc_rp: VARIANT_FLAGS = -gc -region_profile $(PROFILE_LOG_FLAGS)
mlkit_basislib_par_rp mlkit_kitlib_par_rp: VARIANT_FLAGS = -no_gc -par -region_profile $(PROFILE_LOG_FLAGS)
mlkit_basislib_ri_rp mlkit_kitlib_ri_rp: VARIANT_FLAGS = -no_gc -region_profile $(PROFILE_LOG_FLAGS)

mlkit_basislib_ri: REML_VARIANT_FLAGS = -no_par
mlkit_basislib_par: REML_VARIANT_FLAGS = -par
mlkit_basislib_ri_rp: REML_VARIANT_FLAGS = -no_par -rp $(PROFILE_LOG_FLAGS)
mlkit_basislib_par_rp: REML_VARIANT_FLAGS = -par -rp $(PROFILE_LOG_FLAGS)

# The compiler creates directories with a check followed by mkdir. Create the
# shared parents before starting jobs; each variant has its own subdirectory.
mlkit_basislibs_dirs:
	mkdir -p MLB io/MLB ../kitlib/MLB

$(MLKIT_BASIS_TARGETS): | mlkit_basislibs_dirs
	SML_LIB=.. $(MLKIT) -c $(VARIANT_FLAGS) repl.mlb
	SML_LIB=.. $(MLKIT) -c $(VARIANT_FLAGS) ../kitlib/region-profile.mlb
	SML_LIB=.. $(MLKIT) -c $(VARIANT_FLAGS) ../kitlib/time-profile.mlb
	$(if $(REML_VARIANT_FLAGS),SML_LIB=.. $(REML) -c $(REML_VARIANT_FLAGS) reml.mlb)

$(MLKIT_KIT_TARGETS): mlkit_kitlib_%: mlkit_basislib_%
	SML_LIB=.. $(MLKIT) -c $(VARIANT_FLAGS) kitlib.mlb
