(* This file is piped through sml/nj to build the Kit version for
 * the rpm x86-linux distribution. Code for the piping is
 * in ./Makefile. *)

SMLofNJ.Internals.GC.messages false;
CM.SymVal.define("KIT_TARGET_X86", 1);
CM.make(); 
K.Flags.lookup_flag_entry "enable_lambda_backend" := true;
K.Flags.lookup_flag_entry "garbage_collection" := false;
K.Flags.lookup_flag_entry "delete_target_files" := false;
K.build_basislib();
K.install_x86_linux();
