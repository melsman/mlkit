      structure Posix_File_Sys= 
        struct
        structure O = 
          struct
            val append   =  0wx1
            val excl     =  0wx2
            val noctty   =  0wx4
            val nonblock =  0wx8
            val sync     = 0wx10
            val trunc    = 0wx20
          end

        structure S : sig val irwxu : word
                          val irusr : word
                          val iwusr : word
                          val ixusr : word
                          val irwxg : word
                          val irgrp : word
                          val iwgrp : word
                          val ixgrp : word
                          val irwxo : word
                          val iroth : word
                          val iwoth : word
                          val ixoth : word
                          val isuid : word
                          val isgid : word
                       end  = 
          struct
            val irwxu =    0wx1
            val irusr =    0wx2
            val iwusr =    0wx4
            val ixusr =    0wx8
            val irwxg =   0wx10
            val irgrp =   0wx20
            val iwgrp =   0wx40
            val ixgrp =   0wx80
            val irwxo =  0wx100
            val iroth =  0wx200
            val iwoth =  0wx400
            val ixoth =  0wx800
            val isuid = 0wx1000
            val isgid = 0wx2000
          end
        end
 

