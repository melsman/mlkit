structure UlFile =
  struct
    type scripts = string
    type location = string
    type uofile = string
    datatype UlSyntax = UlFile of (scripts * location) list
                      | UoFile of uofile list
                      | Script of (uofile * location option) list
  end
