      structure Posix_File_Sys = 
        struct
        structure O = 
          struct
            type flags = SysWord.word
             
            val append   =  0wx1
            val excl     =  0wx2
            val noctty   =  0wx4
            val nonblock =  0wx8
            val sync     = 0wx10
            val trunc    = 0wx20

            fun toWord x = x
            val fromWord = toWord
            val flags = List.foldl SysWord.orb 0wx0
            val all = flags [append,excl,noctty,nonblock,sync,trunc]
            val intersect = List.foldl SysWord.andb 0wx3F
            fun clear (f1,f2) = SysWord.andb (SysWord.notb f1,f2)
            fun allSet (f1,f2) = SysWord.andb (f1,f2) = f1
            fun anySet (f1,f2) = SysWord.andb (f1,f2) <> 0wx0
          end

        structure S = 
          struct
            type mode = SysWord.word
            type flags = mode

            val irwxu = 0wx1
            val irusr = 0wx2
            val iwusr = 0wx4
            val ixusr = 0wx8
            val irwxg = 0wx10
            val irgrp = 0wx20
            val iwgrp = 0wx40
            val ixgrp = 0wx80
            val irwxo = 0wx100
            val iroth = 0wx200
            val iwoth = 0wx400
            val ixoth = 0wx800
            val isuid = 0wx1000
            val isgid = 0wx2000

            fun toWord x = x
            val fromWord = toWord
            val flags = List.foldl SysWord.orb 0wx0
            val all = flags [irwxu,irusr,iwusr,ixusr,irwxg,irgrp,iwgrp,
                             ixgrp,irwxo,iroth,iwoth,ixoth,isuid,isgid]
            val intersect = List.foldl SysWord.andb 0wx3F
            fun clear (f1,f2) = SysWord.andb (SysWord.notb f1,f2)
            fun allSet (f1,f2) = SysWord.andb (f1,f2) = f1
            fun anySet (f1,f2) = SysWord.andb (f1,f2) <> 0wx0
          end
        end
 

