-- Common mime types (administered from admin pages)
--
-- see http://www.isi.edu/in-notes/iana/assignments/media-types/
-- also http://www.utoronto.ca/webdocs/HTMLdocs/Book/Book-3ed/appb/mimetype.html
--
-- data assembly Jeff Davis davis@xarg.net 

-- Here are Mime types + text description + cannonical extension
--
-- mapping of extension to mime type done later.

-- This is a modified version of the mine-type-data.sql file from
-- OpenAcs v. 6.3 beta 1.

insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval,'Unkown'                  , '*/*'                           , '' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'AutoCAD drawing files'   , 'application/acad'              , 'dwg' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Andrew data stream'      , 'application/andrew-inset'      , 'ez' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'ClarisCAD files'         , 'application/clariscad'         , 'ccad' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Text - Comma separated value'   , 'application/csv'               , 'csv' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'MATRA Prelude drafting'  , 'application/drafting'          , 'drw' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'DXF (AutoCAD)'           , 'application/dxf'               , 'dxf' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Filemaker Pro'           , 'application/filemaker'         , 'fm' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Macromedia Futuresplash' , 'application/futuresplash'      , 'spl' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'NCSA HDF data format'    , 'application/hdf'               , 'hdf' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Image - IGES graphics format'    , 'application/iges'              , 'iges' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Mac binhex 4.0'          , 'application/mac-binhex40'      , 'hqx' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Mac Compactpro'          , 'application/mac-compactpro'    , 'cpt' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Microsoft Word'          , 'application/msword'            , 'doc' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Uninterpreted binary'    , 'application/octet-stream'      , 'bin' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'ODA ODIF'                , 'application/oda'               , 'oda' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension,file_icon) 
  values (scs_fs_mime_types_id_seq.nextval, 'PDF', 'application/pdf', 'pdf', '/ucs/images/file_icon.gif');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'PostScript'              , 'application/postscript'        , 'ps' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'RTF - Rich Text Format'  , 'application/rtf'               , 'rtf' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Stereolithography'       , 'application/sla'               , 'stl');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'VCard'                   , 'application/vcard'             , 'vcf');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'VDA-FS Surface data'     , 'application/vda'               , 'vda');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'SSOYE Koan Files'        , 'application/vnd.koan'          , 'skp');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'FrameMaker MIF format'   , 'application/vnd.mif'           , 'mif' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Microsoft Access file'   , 'application/vnd.ms-access'     , 'mdb' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Microsoft Excel'         , 'application/vnd.ms-excel'      , 'xls' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Microsoft PowerPoint'    , 'application/vnd.ms-powerpoint' , 'ppt' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Microsoft Project'       , 'application/vnd.ms-project'    , 'mpp' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'WML XML in binary format', 'application/vnd.wap.wmlc'      , 'wmlc');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'WMLScript bytecode'      , 'application/vnd.wap.wmlscriptc', 'wmlsc');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'CorelXARA'               , 'application/vnd.xara'          , 'xar');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'WordPerfect'             , 'application/wordperfect'       , 'wpd');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'WordPerfect 6.0'         , 'application/wordperfect6.0'    , 'w60');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Archive ARJ '            , 'application/x-arj-compressed'  , 'arj');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Macromedia Authorware'   , 'application/x-authorware-bin'  , 'aab' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Macromedia Authorware'   , 'application/x-authorware-map'  , 'aam' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Macromedia Authorware'   , 'application/x-authorware-seg'  , 'aas' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Channel Definition'      , 'application/x-cdf'             , 'cdf' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'VCD'                     , 'application/x-cdlink'          , 'vcd' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Chess PGN file'          , 'application/x-chess-pgn'       , 'pgn');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Archive compres'         , 'application/x-compress'        , 'z');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Archive CPIO'            , 'application/x-cpio'            , 'cpio');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'C-shell script'          , 'application/x-csh'             , 'csh' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Archive Debian Package'  , 'application/x-debian-package'  , 'deb');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Macromedia Director'     , 'application/x-director'        , 'dxr' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'TeX DVI file'            , 'application/x-dvi'             , 'dvi' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Archive GNU Tar'         , 'application/x-gtar'            , 'gtar');
insert into scs_fs_mime_types (id,label,mime_type,file_extension,file_icon) 
  values (scs_fs_mime_types_id_seq.nextval, 'Archive gzip compressed' , 'application/x-gzip'            , 'gz', '/ucs/images/file_icon.gif');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'CGI Script'              , 'application/x-httpd-cgi'       , 'cgi');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Adobe Illustrator'       , 'application/x-illustrator'     , 'ai' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Installshield data'      , 'application/x-installshield'   , 'wis');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Java Network Launching Protocol', 'application/x-java-jnlp-file', 'jnlp');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Javascript'              , 'application/x-javascript'      , 'js' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'LaTeX source'            , 'application/x-latex'           , 'latex' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Windows Media Services (wmd)', 'application/x-ms-wmd'      , 'wmd');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Windows Media Services (wmz)', 'application/x-ms-wmz'      , 'wmz');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Audio Ogg Vorbis'        , 'application/x-ogg'             , 'ogg' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Adobe PageMaker'         , 'application/x-pagemaker'       , 'p65' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Photoshop'               , 'application/x-photoshop'       , 'psd' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Palm Pilot Data'         , 'application/x-pilot'           , 'prc' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Audio Real'              , 'application/x-pn-realmedia'    , 'rp');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Quattro Pro'             , 'application/x-quattro-pro'     , 'wq1');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Archive RAR'             , 'application/x-rar-compressed'  , 'rar');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Session Description Protocol', 'application/sdp'           , 'sdp' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Macromedia Shockwave'    , 'application/x-shockwave-flash' , 'swf' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'SQL'                     , 'application/x-sql'             , 'sql' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Archive Mac Stuffit compressed'  , 'application/x-stuffit'         , 'sit' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Archive SVR4 cpio'       , 'application/x-sv4cpio'         , 'sv4cpio');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Archive SVR4 crc'        , 'application/x-sv4crc'          , 'sv4crc');
insert into scs_fs_mime_types (id,label,mime_type,file_extension,file_icon) 
  values (scs_fs_mime_types_id_seq.nextval, 'Archive Tar'             , 'application/x-tar'             , 'tar', '/ucs/images/file_icon.gif');
insert into scs_fs_mime_types (id,label,mime_type,file_extension,file_icon) 
  values (scs_fs_mime_types_id_seq.nextval, 'Archive Zipped Tar'      , 'application/x-zipped-tar'             , 'tgz', '/ucs/images/file_icon.gif');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Text - TeX source'         , 'application/x-tex'             , 'tex' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Text - Texinfo (emacs)'    , 'application/x-texinfo'         , 'texinfo' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Text - troff'              , 'application/x-troff'           , 'tr' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Text - troff with MAN macros'   , 'application/x-troff-man'       , 'man' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Text - troff with ME macros'    , 'application/x-troff-me'        , 'me' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Text - troff with MS macros'    , 'application/x-troff-ms'        , 'ms' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Archive POSIX Tar'       , 'application/x-ustar'           , 'ustar');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'X509 CA Cert'            , 'application/x-x509-ca-cert'    , 'cacert');
insert into scs_fs_mime_types (id,label,mime_type,file_extension,file_icon) 
  values (scs_fs_mime_types_id_seq.nextval, 'Archive Zip'             , 'application/zip'               , 'zip', '/ucs/images/file_icon.gif');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Basic audio (m-law PCM)' , 'audio/basic'                   , 'au' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Audio Midi'              , 'audio/midi'                    , 'midi');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Audio MPEG'              , 'audio/x-mpeg'                  , 'mp3');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Audio MPEG-2'            , 'audio/x-mpeg2'                 , 'mp2a');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Audio Java Media Framework', 'audio/rmf'                   , 'rmf'); 
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Audio Voice'             , 'audio/voice'                   , 'voc' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Audio AIFF'              , 'audio/x-aiff'                  , 'aif' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Audio Mod'               , 'audio/x-mod'                   , 'xm');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Audio mpeg url (m3u)'    , 'audio/x-mpegurl'               , 'm3u');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Audio Windows Media Services (wma)', 'audio/x-ms-wma'            , 'wma');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Audio Windows Media Services (wmv)', 'audio/x-ms-wmv'            , 'wmv');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Audio Realaudio'         , 'audio/x-pn-realaudio'          , 'ra' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Audio Realaudio Plugin'  , 'audio/x-pn-realaudio-plugin'   , 'rm' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Audio Microsoft WAVE'    , 'audio/x-wav'                   , 'wav' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Chemical Brookhaven PDB' , 'chemical/x-pdb'                , 'pdb');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Chemical XMol XYZ'       , 'chemical/x-xyz'                , 'xyz');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'WHIP Web Drawing file'   , 'drawing/x-dwf'                 , 'dwf');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Image - BMP'             , 'image/bmp'                     , 'bmp' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Image - Fractal Image Format', 'image/fif'                     , 'fif');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Image - Gif'             , 'image/gif'                     , 'gif' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Image - Image Exchange Format' , 'image/ief'                     , 'ief' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Image - Jpeg'            , 'image/jpeg'                    , 'jpg' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Image - PNG'             , 'image/png'                     , 'png' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Image - TIFF'            , 'image/tiff'                    , 'tif' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Image - WAP wireless bitmap'     , 'image/vnd.wap.wbmp'            , 'wbmp');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Image - CMU Raster'      , 'image/x-cmu-raster'            , 'ras' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Image - Flexible Image Transport', 'image/x-fits'                  , 'fit' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Image - Macromedia Freehand'     , 'image/x-freehand'              , 'fh' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Image - SVG'             , 'image/xml+svg'                 , 'svg' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Image - PhotoCD'         , 'image/x-photo-cd'              , 'pcd' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Image - Mac pict'        , 'image/x-pict'                  , 'pict' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Image - PNM'             , 'image/x-portable-anymap'       , 'pnm' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Image - PBM'             , 'image/x-portable-bitmap'       , 'pbm' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Image - PGM'             , 'image/x-portable-graymap'      , 'pgm' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Image - Portable Pixmap' , 'image/x-portable-pixmap'       , 'ppm');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Image - RGB'             , 'image/x-rgb'                   , 'rgb');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Image - X bitmap'        , 'image/x-xbitmap'               , 'xbm' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Image - X pixmap'        , 'image/x-xpixmap'               , 'xpm' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Image - X window dump (xwd)' , 'image/x-xwindowdump'           , 'xwd' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'RFC822 Message'          , 'message/rfc822'                , 'mime');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Computational mesh'      , 'model/mesh'                    , 'mesh');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Text - SGML Text'        , 'text/sgml'                     , 'sgml');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Text - CSS'              , 'text/css'                      , 'css' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Text - HTML'             , 'text/html'                     , 'html' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Text - Plain text'       , 'text/plain'                    , 'txt' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Text - Plain text (flowed)' , 'text/plain; format=flowed'     , 'text' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Text - Enriched Text'    , 'text/enriched'                 , 'rtx' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Text - Tab separated values'    , 'text/tab-separated-values'     , 'tsv' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Text - WMLScript'        , 'text/vnd.wap.wmlscript'        , 'wmls');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Text - WML'              , 'text/vnd.wap.wml'              , 'wml');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Text - XML Document'     , 'text/xml'                      , 'xml' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Text - Structured enhanced text', 'text/x-setext'                 , 'etx');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Text - XSL'              , 'text/xsl'                      , 'xsl' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Video FLI'               , 'video/fli'                     , 'fli');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Video MPEG'              , 'video/mpeg'                    , 'mpg' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Video MPEG-2'            , 'video/mpeg2'                   , 'mpv2' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Video Quicktime'         , 'video/quicktime'               , 'mov' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Video VDOlive streaming' , 'video/vdo'                     , 'vdo');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Video Vivo'              , 'video/vnd.vivo'                , 'vivo');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Video Microsoft ASF'     , 'video/x-ms-asf'                , 'asf' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Video Windows Media Services (wm)', 'video/x-ms-wm'              , 'wm');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Video Windows Media Services (wvx)', 'video/x-ms-wvx'            , 'wvx');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Video Windows Media Services (wmx)', 'video/x-mx-wmx'            , 'wmx');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Video Microsoft AVI'     , 'video/x-msvideo'               , 'avi' );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Video SGI movie player'  , 'video/x-sgi-movie'             , 'movie'  );
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Conference Cooltalk'     , 'x-conference/x-cooltalk'       , 'ice');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'VRML'                    , 'x-world/x-vrml'                , 'vrml');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Xuda'                    , 'xuda/gen-cert'                 , 'xuda');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Enhanced text'            , 'text/enhanced'                 , 'etxt');
insert into scs_fs_mime_types (id,label,mime_type,file_extension) values (scs_fs_mime_types_id_seq.nextval, 'Fixed-width text'         , 'text/fixed-width'              , 'ftxt');


-- Extension to mime type maps.

-- text/plain for prog langs (maybe we should do application/x-LANG but then you can't look
-- at the code in the browser.
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'c', 'text/plain');
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'c++', 'text/plain' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'cpp', 'text/plain' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'cxx', 'text/plain' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'cc', 'text/plain' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'h', 'text/plain' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'hh', 'text/plain' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'h++', 'text/plain' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'hxx', 'text/plain' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'tcl', 'text/plain' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'sql', 'text/plain' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'sh', 'text/plain' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'csh', 'text/plain' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'ksh', 'text/plain' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'py', 'text/plain' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'java', 'text/plain' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'xql', 'text/plain' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'php', 'text/plain' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'm4', 'text/plain' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'pl', 'text/plain' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'pm', 'text/plain' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'pod', 'text/plain' );

-- -- map a few to binary 
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'o','application/octet-stream' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'so','application/octet-stream' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'a','application/octet-stream' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'dll','application/octet-stream' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'exe','application/octet-stream' );

-- -- all the rest
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'aab','application/x-authorware-bin' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'aam','application/x-authorware-map' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'aas','application/x-authorware-seg' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'ai','application/x-illustrator');
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'aif','audio/x-aiff' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'aifc','audio/x-aiff' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'aiff','audio/x-aiff' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'ani','application/octet-stream' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'arj','application/x-arj-compressed' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'asc','text/plain' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'asf','video/x-ms-asf' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'asx','video/x-ms-asf' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'au','audio/basic' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'avi','video/x-msvideo' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'bin','application/octet-stream' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'bmp','image/bmp' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'bqy','application/octet-stream' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'cacert','application/x-x509-ca-cert' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'ccad','application/clariscad' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'cdf','application/x-netcdf' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'cgi','application/x-httpd-cgi' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'class','application/octet-stream' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'cpio','application/x-cpio' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'cpt','application/mac-compactpro' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'css','text/css' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'csv','application/csv');
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'cur','application/octet-stream' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'dcr','application/x-director' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'deb','application/x-debian-package' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'dhtml','text/html' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'dir','application/x-director' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'dms','application/octet-stream' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'doc','application/msword' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'dot','application/msword' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'drw','application/drafting' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'dump','application/octet-stream' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'dvi','application/x-dvi' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'dwf','drawing/x-dwf' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'dwg','application/acad' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'dxf','application/dxf' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'dxr','application/x-director' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'eps','application/postscript' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'etx','text/x-setext' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'ez','application/andrew-inset' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'fh4','image/x-freehand' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'fh5','image/x-freehand' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'fh7','image/x-freehand' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'fhc','image/x-freehand' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'fh','image/x-freehand' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'fif','image/fif' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'fit','image/x-fits');
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'fli','video/fli' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'fm','application/filemaker');
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'gif','image/gif' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'gtar','application/x-gtar' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'gz','application/x-gzip' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'gzip','application/x-gzip' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'hdf','application/hdf');
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'hqx','application/mac-binhex40' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'html','text/html' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'htm','text/html' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'ice','x-conference/x-cooltalk' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'ico','application/octet-stream' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'ief','image/ief' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'iges','application/iges' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'igs','application/iges' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'jnlp','application/x-java-jnlp-file' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'jpeg','image/jpeg' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'jpe','image/jpeg' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'jpg','image/jpeg' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'js','application/x-javascript' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'kar','audio/midi' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'latex','application/x-latex' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'lha','application/octet-stream' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'lzh','application/octet-stream' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'm15','audio/x-mod' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'm3u','audio/x-mpegurl' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'm3url','audio/x-mpegurl' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'man','application/x-troff-man' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'mdb','application/vnd.ms-access');
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'me','application/x-troff-me' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'mesh','model/mesh' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'mid','audio/midi' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'midi','audio/midi' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'mif','application/vnd.mif' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'mime','message/rfc822' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'movie','video/x-sgi-movie' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'mov','video/quicktime' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'mp2','audio/x-mpeg2' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'mp2a','audio/x-mpeg2' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'mp3','audio/x-mpeg' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'mp3a','audio/x-mpeg' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'mpeg','video/mpeg' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'mpe','video/mpeg' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'mpga','audio/x-mpeg' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'mpg','video/mpeg' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'mpv2','video/mpeg2' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'mp2v','video/mpeg2' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'mpp','application/vnd.ms-project');
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'mpc','application/vnd.ms-project');
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'mpt','application/vnd.ms-project');
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'mpx','application/vnd.ms-project');
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'mpw','application/vnd.ms-project');
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'ms','application/x-troff-ms' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'msh','model/mesh' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'msw','application/msword' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'mtm','audio/x-mod' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'nc','application/x-netcdf' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'oda','application/oda' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'ogg','application/x-ogg');
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'p65','application/x-pagemaker');
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'pbm','image/x-portable-bitmap' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'pcd','image/x-photo-cd');
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'pdb','chemical/x-pdb' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'pdf','application/pdf' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'pgm','image/x-portable-graymap' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'pgn','application/x-chess-pgn' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'pict','image/x-pict' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'png','image/png' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'pnm','image/x-portable-anymap' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'ppm','image/x-portable-pixmap' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'ppt','application/vnd.ms-powerpoint' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'ppz','application/vnd.ms-powerpoint' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'pps','application/vnd.ms-powerpoint' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'pot','application/vnd.ms-powerpoint' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'prc','application/x-pilot');
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'ps','application/postscript' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'psd','application/x-photoshop');
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'qt','video/quicktime' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'ra','audio/x-pn-realaudio' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'ram','audio/x-pn-realaudio' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'rar','application/x-rar-compressed' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'ras','image/x-cmu-raster' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'rgb','image/x-rgb' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'rmf', 'audio/rmf');
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'rm','audio/x-pn-realaudio-plugin' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'rmm','audio/x-pn-realaudio-plugin' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'roff','application/x-troff' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'rp','application/x-pn-realmedia' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'rpm','audio/x-pn-realaudio-plugin' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'rr','application/x-troff' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'rtf','application/rtf' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'rtx','text/enriched' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 's3m','audio/x-mod' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'sd2','application/octet-stream' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'sdp','application/sdp' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'sea','application/x-stuffit' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'sgml','text/sgml' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'sgm','text/sgml' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'shtml','text/html' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'silo','model/mesh' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'sit','application/x-stuffit' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'skd','application/vnd.koan' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'skm','application/vnd.koan' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'skp','application/vnd.koan' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'skt','application/vnd.koan' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'snd','audio/basic' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'spl','application/futuresplash' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'stl','application/sla' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'stm','audio/x-mod' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'sv4cpio','application/x-sv4cpio' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'sv4crc','application/x-sv4crc' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'svg','image/xml+svg');
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'swf','application/x-shockwave-flash' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 't','application/x-troff' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'tar','application/x-tar' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'tex','application/x-tex' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'texi','application/x-texinfo' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'texinfo','application/x-texinfo' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'text','text/plain; format=flowed');
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'tiff','image/tiff' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'tif','image/tiff' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'tr','application/x-troff' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'tsv','text/tab-separated-values' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'txt','text/plain' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'ult','audio/x-mod' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'ustar','application/x-ustar' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'uu','application/octet-stream' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'vcd','application/x-cdlink' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'vcf','application/vcard' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'vdo','video/vdo' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'vda','application/vda' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'vivo','video/vnd.vivo' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'viv','video/vnd.vivo' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'voc','audio/voice');
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'vrml','x-world/x-vrml' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'vrm','x-world/x-vrml' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'wav','audio/x-wav' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'wb1','application/x-quattro-pro' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'wb2','application/x-quattro-pro' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'wb3','application/x-quattro-pro' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'wbmp','image/vnd.wap.wbmp' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'web','application/vnd.xara' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'wis','application/x-installshield' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'wma','audio/x-ms-wma' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'wmd','application/x-ms-wmd' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'wmlc','application/vnd.wap.wmlc' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'wmlsc','application/vnd.wap.wmlscriptc' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'wmls','text/vnd.wap.wmlscript' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'wml','text/vnd.wap.wml' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'wmv','audio/x-ms-wmv' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'wm','video/x-ms-wm' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'wmx','video/x-mx-wmx' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'wmz','application/x-ms-wmz' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'wpd','application/wordperfect' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'wq1','application/x-quattro-pro' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'wrl','x-world/x-vrml' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'wvx','video/x-ms-wvx' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'xar','application/vnd.xara' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'w60','application/wordperfect6.0');
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'xbm','image/x-xbitmap' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'xlc','application/vnd.ms-excel' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'xls','application/vnd.ms-excel' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'xlm','application/vnd.ms-excel' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'xlw','application/vnd.ms-excel' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'xm','audio/x-mod' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'xml','text/xml' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'xpm','image/x-xpixmap' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'xsl','text/xsl' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'xuda','xuda/gen-cert' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'xwd','image/x-xwindowdump' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'xyz','chemical/x-xyz' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'z','application/x-compress' );
-- insert into cr_extension_mime_type_map (extension, mime_type) values (scs_fs_mime_types_id_seq.nextval, 'zip','application/zip' );


-- --  Here are some less common mime types and extensions not defined here.
-- --
-- --  tsp       | application/dsptype
-- --  pfr       | application/font-tdpfr
-- --  imd       | application/immedia
-- --  mbd       | application/mbedlet
-- --  pps       | application/pps
-- --  prt       | application/pro_eng
-- --  smi       | application/smil
-- --  smil      | application/smil
-- --  sol       | application/solids
-- --  step      | application/step
-- --  stp       | application/step
-- --  vmd       | application/vocaltec-media-desc
-- --  vmf       | application/vocaltec-media-file
-- --  bcpio     | application/x-bcpio
-- --  chat      | application/x-chat
-- --  ipx       | application/x-ipix
-- --  ips       | application/x-ipscript
-- --  src       | application/x-wais-source
-- --  wsrc      | application/x-wais-source
-- --  vox       | audio/voxware
-- --  rmf       | audio/x-rmf
-- --  svh       | image/svh
-- --  ivr       | i-world/i-vrml
-- --  hdml      | text/x-hdml
