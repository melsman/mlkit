
signature OS = 
  sig
    structure Path : OS_PATH
  end

structure OS : OS = 
  struct
    structure Path = Path
  end

