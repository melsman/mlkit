#---------------------------------------
# Sample AOLserver configuration file
# with SMLserver and Postgresql support
#---------------------------------------

set user yourlogin
set port 8080
set pg_passwd XXXX

set webdir /home/${user}/web
set nssml_so ../../smlserver/bin/nssml.so
set home /usr/share/aolserver

set host [ns_info hostname]
set bindir [file dirname [ns_info nsd]]

ns_section "ns/mimetypes"
ns_param .wml text/vnd.wap.wml 
ns_param .wbmp image/vnd.wap.wbmp 
ns_param .wmls text/vnd.wap.wmlscript 
ns_param .wmlc application/vnd.wap.wmlc 
ns_param .wmlsc application/vnd.wap.wmlscriptc 

ns_section "ns/parameters"
ns_param debug off
ns_param Home $home
ns_param serverlog ${webdir}/log/server.log
ns_param pidfile ${webdir}/log/nspid.txt
ns_param user ${user}
ns_param stacksize 500000

ns_section "ns/servers"
ns_param ${user} "${user}'s server"

ns_section "ns/server/${user}"
ns_param directoryfile "index.sml"
ns_param pageroot ${webdir}/www
ns_param enabletclpages off

ns_section "ns/server/${user}/module/nslog"
ns_param file ${webdir}/log/access.log

ns_section "ns/server/${user}/module/nssock"
ns_param port ${port}
ns_param hostname $host

ns_section "ns/server/${user}/module/nssml"
ns_param prjid sources

# Initialization script to schedule script execution and 
# register trapping of URL requests; remember to include
# the initialization script in the project file.
#ns_param initscript ../sys/init.sml

# 
# Database drivers 
#
ns_section "ns/db/drivers" 
ns_param postgres /usr/share/pgdriver/bin/postgres.so

ns_section "ns/db/pools" 
ns_param pg_main "pg_main" 
ns_param pg_sub "pg_sub" 

ns_section "ns/db/pool/pg_main" 
ns_param Driver postgres 
ns_param Connections 5
ns_param DataSource localhost::${user}
ns_param User ${user}
ns_param Password ${pg_passwd}
ns_param Verbose Off
ns_param LogSQLErrors On 
ns_param ExtendedTableInfo On 

ns_section "ns/db/pool/pg_sub" 
ns_param Driver postgres 
ns_param Connections 5 
ns_param DataSource localhost::${user} 
ns_param User ${user} 
ns_param Password ${pg_passwd}
ns_param Verbose Off
ns_param LogSQLErrors On 
ns_param ExtendedTableInfo On 

ns_section "ns/server/${user}/db" 
ns_param Pools pg_main,pg_sub
ns_param DefaultPool "pg_main"

ns_section "ns/server/${user}/modules"
ns_param nssock nssock.so
ns_param nslog nslog.so
ns_param nssml ${nssml_so}
