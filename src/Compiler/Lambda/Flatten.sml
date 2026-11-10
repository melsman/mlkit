structure Flatten : FLATTEN where type LambdaPgm = LambdaExp.LambdaPgm =
struct
  type LambdaPgm = LambdaExp.LambdaPgm

  fun option (name,description) =
      Flags.add_bool_entry
        {long=name,short=NONE,menu=["Optimiser Control",name],
         item=ref false,neg=true,
         desc=description ^ " Data-flattening rewrites are not implemented yet."}

  val enabled = option ("data_flattening","Enable data flattening of heap records.")
  val refs = option ("data_flattening_refs","Enable reference data flattening.")
  val tuples = option ("data_flattening_tuples","Enable tuple data flattening.")
  val datatypes = option ("data_flattening_datatypes","Enable datatype layout specialisation.")
  val floats = option ("data_flattening_floats","Enable float data flattening.")

  (* This is the pipeline boundary for the later analysis and rewriting.
     No representation variables or exported optimiser environments change.
     Consequently these switches do not affect cache compatibility until a
     rewrite is implemented; enabling one then requires a cache-key update. *)
  fun optimise {enabled=global} pgm =
      if global andalso enabled() then pgm else pgm
end
