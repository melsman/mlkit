#
# The contents of this file are subject to the AOLserver Public License
# Version 1.1 (the "License"); you may not use this file except in
# compliance with the License. You may obtain a copy of the License at
# http://aolserver.com/.
#
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
#
# The Original Code is AOLserver Code and related documentation
# distributed by AOL.
# 
# The Initial Developer of the Original Code is America Online,
# Inc. Portions created by AOL are Copyright (C) 1999 America Online,
# Inc. All Rights Reserved.
#
# Copyright (C) 2001 Scott S. Goodwin
#
# Derived from http.tcl, originally written by AOL
#
# Alternatively, the contents of this file may be used under the terms
# of the GNU General Public License (the "GPL"), in which case the
# provisions of GPL are applicable instead of those above.  If you wish
# to allow use of your version of this file only under the terms of the
# GPL and not to allow others to use your version of this file under the
# License, indicate your decision by deleting the provisions above and
# replace them with the notice and other provisions required by the GPL.
# If you do not delete the provisions above, a recipient may use your
# version of this file under either the License or the GPL.
#

#
# $Header$
#

# https.tcl -
#	Routines for opening non-blocking HTTPS (SSL) connections through
#	the Tcl socket interface. This is essentially a copy of http.tcl
#       with appropriate changes.
#

#
# ==========================================================================
# API procs
# ==========================================================================

#
# ns_httpsopen -
#	Fetch a web page
#
# Results:
#	A tcl list {read file handle} {write file handle} {result headers set}
#       {name of the module}
#
# Side effects:
#	May throw an error on failure.
#

proc ns_httpsopen {method url {rqset ""} {timeout 30} {pdata ""} {module ""}} {
    #
    # Determine if url is local; prepend site address if so. Not
    # pretty but it'll do for now.
    #             

    if [string match /* $url] {
	if {$module == ""} {
	    set module "nsopenssl"
	}
	if {"[ns_config ns/server/[ns_info server]/module/$module ServerLocation]" != ""} {
	    set host [ns_config ns/server/[ns_info server]/module/$module ServerLocation]
	} elseif {"[ns_config ns/server/[ns_info server]/module/$module ServerHostname]" != ""} {
	    set host "https://[ns_config ns/server/[ns_info server]/module/$module ServerHostname]"
	} elseif {"[ns_config ns/server/[ns_info server]/module/$module ServerAddress]" != ""} {
	    set host "https://[ns_config ns/server/[ns_info server]/module/$module ServerAddress]"
	} else {
	    ns_log error "ns_httpsopen: you need to set ServerLocation, \
ServerHostname or ServerAddress in the configuration file for nsopenssl"
	}
        set port [ns_config ns/server/[ns_info server]/module/$module ServerPort]   
        if { $port != 443 } {
            append host ":$port"
        }
        set url "$host$url"
#        ns_log notice "ADJUSTED URL: $url"                    
    }   

    #
    # Verify that the URL is an HTTPS url.
    #

    if ![string match https://* $url] {
	return -code error "Invalid url \"$url\": "\
		"ns_httpsopen only supports HTTPS"
    }

    #
    # Find each element in the URL
    #
    
    set url [split $url /]
    set hp [split [lindex $url 2] :]
    set host [lindex $hp 0]
    set port [lindex $hp 1]
    if [string match $port ""] {
	set port 443
    }
    set uri /[join [lrange $url 3 end] /]

    #
    # Open a TCP connection to the host:port
    #
   
    set fds [ns_openssl_sockopen -nonblock $host $port]
    set rfd [lindex $fds 0]
    set wfd [lindex $fds 1]
    if [catch {
	#
	# First write the request, then the headers if they exist.
	#
	
	_ns_https_puts $timeout $wfd "$method $uri HTTP/1.0\r"
	
	if {$rqset != ""} {
	    #
	    # There are request headers
	    #

	    # XXX should check to see if req'd Accept and User-Agent headers are there */

	    for {set i 0} {$i < [ns_set size $rqset]} {incr i} {
		set key [ns_set key $rqset $i]
		set val [ns_set value $rqset $i]
		_ns_https_puts $timeout $wfd "$key: $val\r"
	    }
	} else {
	    #
	    # No headers were specified, so send a minimum set of
	    # required headers.
	    #

	    _ns_https_puts $timeout $wfd "Accept: */*\r"
	    _ns_https_puts $timeout $wfd \
		    "User-Agent: [ns_info name]-Tcl/[ns_info version]\r"
	}

	#
	# Always send a Host: header because virtual hosting happens
	# even with HTTP/1.0.
	#
	
	if { $port == 443 } {
	    set hostheader "Host: ${host}\r"
	} else {
	    set hostheader "Host: ${host}:${port}\r"
	}
	_ns_https_puts $timeout $wfd $hostheader

	#
	# If optional content exists, then output that. Otherwise spit
	# out a newline to end the headers.
	#
	
	if {$pdata != ""} {
	    _ns_https_puts $timeout $wfd "\r\n$pdata\r"
	} else {
	    _ns_https_puts $timeout $wfd "\r"
	}
	flush $wfd

	#
	# Create a new set; its name will be the result line from
	# the server. Then read headers into the set.
	#
	
	set rpset [ns_set new [_ns_https_gets $timeout $rfd]]
	while 1 {
	    set line [_ns_https_gets $timeout $rfd]
	    if ![string length $line] {
		break
	    }
	    ns_parseheader $rpset $line
	}
    } errMsg] {
	#
	# Something went wrong during the request, so return an error.
	#
	
	global errorInfo
	close $wfd
	close $rfd
	if [info exists rpset] {
	    ns_set free $rpset
	}
	return -code error -errorinfo $errorInfo $errMsg
    }

    #
    # Return a list of read file, write file, and headers set.
    #
    
    return [list $rfd $wfd $rpset]
}

#
# ns_httpspost -
#	Perform a POST request. This wraps ns_httpsopen.
#
# Results:
#	The URL content.
#
# Side effects:
#

proc ns_httpspost {url {rqset ""} {qsset ""} {type ""} {timeout 30}} {
    #
    # Build the request. Since we're posting, we have to set
    # content-type and content-length ourselves. We'll add these to
    # rqset, overwriting if they already existed, which they
    # shouldn't.
    #

    set boundary "-----------------rc029340985544hg24309nto8899o9"

    if {[string match "" $rqset]} { 
	set rqset [ns_set new rqset]
	ns_set put $rqset "Accept" "*/*"
	ns_set put $rqset "User-Agent" "[ns_info name]-Tcl/[ns_info version]"
    }

    if {$type == ""} {
	ns_set put $rqset "Content-type" "application/x-www-form-urlencoded"
    } elseif {$type == "multipart/form-data"} {
	# Can't double-quote the boundary value because of form.tcl
	ns_set put $rqset "Content-type" "multipart/form-data, boundary=$boundary"
    } else {
	ns_set put $rqset "Content-type" "$type"
    }

    #
    # Build the query string to POST with
    #

    set querystring ""

    if {$type == "multipart/form-data"} {
	if {![string match "" $qsset]} {
	    for {set i 0} {$i < [ns_set size $qsset]} {incr i} {
		set key [ns_set key $qsset $i]
		set value [ns_set value $qsset $i]
		append querystring "--${boundary}\r\n"
		append querystring "Content-Disposition: form-data; name=\"$key\"\r\n\r\n"
		append querystring "$value\r\n"
	    }
	    append querystring "--${boundary}--\n"
	    ns_set put $rqset "Content-length" [string length $querystring]
	} else {
	    ns_set put $rqset "Content-length" "0"
	}
    } else {
	if {![string match "" $qsset]} {
	    for {set i 0} {$i < [ns_set size $qsset]} {incr i} {
		set key [ns_set key $qsset $i]
		set value [ns_set value $qsset $i]
		if { $i > 0 } {
		    append querystring "&"
		}
		append querystring "$key=[ns_urlencode $value]"
	    }
	    ns_set put $rqset "Content-length" [string length $querystring]
	} else {
	    ns_set put $rqset "Content-length" "0"
	}
    }

    #
    # Perform the actual request.
    #
    
    set http [ns_httpsopen POST $url $rqset $timeout $querystring]
    set rfd [lindex $http 0]
    close [lindex $http 1]
    set headers [lindex $http 2]

    set length [ns_set iget $headers content-length]
    if [string match "" $length] {
	set length -1
    }
    set err [catch {
	#
	# Read the content.
	#
	
	while 1 {
	    set buf [_ns_http_read $timeout $rfd $length]
	    append page $buf
	    if [string match "" $buf] {
		break
	    }
	    if {$length > 0} {
		incr length -[string length $buf]
		if {$length <= 0} {
		    break
		}
	    }
	}
    } errMsg]

    ns_set free $headers
    close $rfd
    if $err {
	global errorInfo
	return -code error -errorinfo $errorInfo $errMsg
    }
    return $page
}

#
# ns_httpsget -
#	Perform a GET request. This wraps ns_httpsopen, but it also
#	knows how to follow redirects and will read the content into
#	a buffer.
#
# Results:
#	The URL content.
#
# Side effects:
#	Will only follow redirections 10 levels deep.
#

proc ns_httpsget {url {timeout 30} {depth 0} {rqset ""} {module ""}} {
    if {[incr depth] > 10} {
	return -code error "ns_httpsget: Recursive redirection: $url"
    }
    
    #
    # Perform the actual request.
    #
    
    set https [ns_httpsopen GET $url $rqset $timeout "" $module]
    set rfd [lindex $https 0]
    close [lindex $https 1]
    set headers [lindex $https 2]
    set response [ns_set name $headers]
    set status [lindex $response 1]
    if {$status == 302} {
	#
	# The response was a redirect, so free the headers and
	# recurse.
	#
	set location [ns_set iget $headers location]
	if {$location != ""} {
	    ns_set free $headers
	    close $rfd
	    # XXX should put check for https:// here...
	    if {[string first https:// $location] != 0} {
		set url2 [split $url /]
		set hp [split [lindex $url2 2] :]
		set host [lindex $hp 0]
		set port [lindex $hp 1]
		if [string match $port ""] {
                    set port 443
                }
		regexp "^(.*)://" $url match method
		
		set location "$method://$host:$port/$location"
	    }
	    return [ns_httpsget $location $timeout $depth]
	}
    }
    
    set length [ns_set iget $headers content-length]
    if [string match "" $length] {
	set length -1
    }
    set err [catch {
	#
	# Read the content.
	#
	
	while 1 {
	    set buf [_ns_https_read $timeout $rfd $length]
	    append page $buf
	    if [string match "" $buf] {
		break
	    }
	    if {$length > 0} {
		incr length -[string length $buf]
		if {$length <= 0} {
		    break
		}
	    }
	}
    } errMsg]

    ns_set free $headers
    close $rfd
    if $err {
	global errorInfo
	return -code error -errorinfo $errorInfo $errMsg
    }
    return $page
}

# ==========================================================================
# Local procs
# ==========================================================================

#
# _ns_https_readable -
#	Return the number of bytes available to read from a
# 	socket without blocking, waiting up to $timeout seconds for bytes to
# 	arrive if none are currently available.
#
# Results:
#	Number of bytes that are waiting to be read
#

proc _ns_https_readable {timeout sock} {
    set nread [ns_socknread $sock]
    if !$nread {
	set sel [ns_sockselect -timeout $timeout $sock {} {}]
	if [string match "" [lindex $sel 0]] {
	    return -code error "ns_sockreadwait: Timeout waiting for remote"
	}
	set nread [ns_socknread $sock]
    }
    return $nread
}


#
# _ns_https_read -
#	Read upto $length bytes from a socket without blocking.
#
# Results:
#	Up to $length bytes that were read from the socket. May
#	throw and error on EOF.
#

proc _ns_https_read {timeout sock length} {
    set buf ""
    set nread [_ns_https_readable $timeout $sock]
    if {$nread > 0} {
	if {$length > 0 && $length < $nread} {
	    set nread $length
	}
	set buf [read $sock $nread]
    }
    return $buf
}


#
# _ns_https_gets -
#	Carefully read lines, one character at a time, from a
# 	socket to avoid blocking.
#
# Results:
#	One line read from the socket
#

proc _ns_https_gets {timeout sock} {
    set line ""
    set done 0
    while {!$done} {
	set nline [_ns_https_readable $timeout $sock]
	if !$nline {set done 1}
	while {!$done && $nline > 0} {
	    set char [read $sock 1]
	    if {$char == "\n"} {set done 1}
	    append line $char
	    incr nline -1
	}
    }
    string trimright $line
}


#
# _ns_https_puts -
#	Send a string out a socket.  If the socket buffer is
# 	full, wait for up to $timeout seconds.
#
# Results:
#	None.
#
# Side effects:
#	May return with an error code on timeout.
#

proc _ns_https_puts {timeout sock string} {
    if {[lindex [ns_sockselect -timeout $timeout {} $sock {}] 1] == ""} {
	return -code error "ns_socksend: Timeout writing to socket"
    }
    puts $sock $string
}


