
#---------------------------------------
# Sample AOLserver configuration file
# with SMLserver and Postgresql support
#---------------------------------------

set port 8080
set user mael
set webdir /home/${user}/web
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
ns_param directoryfile index.msp
ns_param pageroot ${webdir}/www
ns_param enabletclpages off

ns_section "ns/server/${user}/module/nslog"
ns_param file ${webdir}/log/access.log

ns_section "ns/server/${user}/module/nssock"
ns_param port ${port}
ns_param hostname $host

ns_section "ns/server/${user}/module/nssml"
ns_param prjid web

# 
# Database drivers 
# The database driver is specified here. PostgreSQL driver being loaded.
# Make sure you have the driver compiled and put it in {aolserverdir}/bin
#
#ns_section "ns/db/drivers" 
#ns_param   nspostgres     ${bindir}/nspostgres.so  ;# Load PostgreSQL driver

# 
# Database Pools: This is how AOLserver ``talks'' to the RDBMS. You need three for
# OpenACS: main, log, subquery. Make sure to replace ``yourdb'' and ``yourpassword''
# with the actual values for your db name and the password for it.
# AOLserver can have different pools connecting to different databases and even
# different database servers.
# 
#ns_section "ns/db/pools" 
#ns_param   main       "Main Pool" 
#ns_param   log        "Log Pool" 
#ns_param   subquery   "Subquery Pool"

#ns_section "ns/db/pool/main" 
#ns_param Driver nspostgres 
#ns_param Connections 5                  ;# 5 is a good number. Increase according to your needs
#ns_param DataSource localhost::${user}  ;# Replace 'user' with the name of your database in PG
#ns_param User ${user}                   ;# User and password AOLserver will use to connect
#ns_param Password "hej" 
#ns_param Verbose Off                    ;# Set it to On to see all queries. Good for debugging SQL.
#ns_param LogSQLErrors On 
#ns_param ExtendedTableInfo On 
# ns_param MaxOpen 1000000000            ;# Uncommenting these two cause AOLserver to keep the
# ns_param MaxIdle=1000000000            ;# db connection open. Can be a good thing, up tp your needs.

#ns_section "ns/db/pool/log" 
#    ns_param Driver nspostgres 
#    ns_param Connections 5 
#    ns_param DataSource localhost::${user}
#   ns_param User ${user}
#    ns_param Password "hej" 
#    ns_param Verbose On 
#    ns_param LogSQLErrors On 
#    ns_param ExtendedTableInfo On 
#   # ns_param MaxOpen 1000000000 
#   # ns_param MaxIdle=1000000000 

#ns_section "ns/db/pool/subquery" 
#ns_param Driver nspostgres 
#ns_param Connections 2 
#ns_param DataSource localhost::${user} 
#ns_param User ${user}
#ns_param Password "hej" 
#ns_param Verbose On 
#ns_param LogSQLErrors On 
#ns_param ExtendedTableInfo On
# ns_param MaxOpen 1000000000 
# ns_param MaxIdle=1000000000 

#ns_section "ns/server/${user}/db" 
#ns_param Pools          "*" 
#ns_param DefaultPool    "main"

ns_section "ns/server/${user}/modules"
ns_param nssock nssock.so
ns_param nslog nslog.so
ns_param nssml "../../smlserver/bin/nssml.so"

