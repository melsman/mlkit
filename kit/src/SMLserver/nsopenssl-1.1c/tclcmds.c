/*
 * The contents of this file are subject to the AOLserver Public License
 * Version 1.1 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://aolserver.lcs.mit.edu/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * The Original Code is AOLserver Code and related documentation
 * distributed by AOL.
 *
 * The Initial Developer of the Original Code is America Online,
 * Inc. Portions created by AOL are Copyright (C) 1999 America Online,
 * Inc. All Rights Reserved.
 *
 * Copyright (C) 2000-2001 Scott S. Goodwin
 * Copyright (C) 2000 Rob Mayoff
 *
 * Alternatively, the contents of this file may be used under the terms
 * of the GNU General Public License (the "GPL"), in which case the
 * provisions of GPL are applicable instead of those above.  If you wish
 * to allow use of your version of this file only under the terms of the
 * GPL and not to allow others to use your version of this file under the
 * License, indicate your decision by deleting the provisions above and
 * replace them with the notice and other provisions required by the GPL.
 * If you do not delete the provisions above, a recipient may use your
 * version of this file under either the License or the GPL.
 */

#include "nsopenssl.h"
#include "tclcmds.h"

/*
 * Local Functions
 */

static int NsOpenSSLCmd(ClientData dummy, Tcl_Interp *interp, int argc,
    char **argv);
static void SetResultToX509Name(Tcl_Interp *interp, X509_NAME *name);
static void SetResultToObjectName(Tcl_Interp *interp, ASN1_OBJECT *obj);
static char *ValidTime(ASN1_UTCTIME *tm);
static char *PEMCertificate(X509 *clientcert);
static NsOpenSSLConnection *NsOpenSSLGetConn(Tcl_Interp *interp);


/*
 *----------------------------------------------------------------------
 *
 * NsOpenSSLInterpInit --
 *
 *      Add nsopenssl commands to Tcl interpreter.
 *
 * Results:
 *      NS_OK or NS_ERROR.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

int
NsOpenSSLInterpInit(Tcl_Interp *interp, void *arg)
{
    if (Tcl_CreateCommand(interp, "ns_openssl", NsOpenSSLCmd, NULL, NULL)
	    == NULL) {
	return NS_ERROR;
    } else {
	return NS_OK;
    }
}


/*
 *----------------------------------------------------------------------
 *
 * NsOpenSSLCmd --
 *
 *      Returns information about nsopenssl, client certificates.
 *
 * Results:
 *      Tcl string result.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

static int
NsOpenSSLCmd(ClientData dummy, Tcl_Interp * interp, int argc,	
    char **argv)
{
    NsOpenSSLConnection *scPtr;
    X509                *clientcert;
    SSL_CIPHER          *cipher;
    char                *string;
    int                  integer;
    int                  status;

    if (argc < 2) {
	Tcl_AppendResult (interp, "wrong # args:  should be \"",
			  argv[0], " command \"", NULL);
	status = TCL_ERROR;
    } else {
	status = TCL_OK;
    }

    scPtr = NsOpenSSLGetConn(interp);

    if (STREQ (argv[1], "info")) {

	Tcl_AppendElement(interp, SSL_LIBRARY_NAME);
	Tcl_AppendElement(interp, SSL_LIBRARY_VERSION);
	Tcl_AppendElement(interp, SSL_CRYPTO_LIBRARY_NAME);
	Tcl_AppendElement(interp, SSL_CRYPTO_LIBRARY_VERSION);

    } else if (STREQ(argv[1], "protocol")) {

        switch(scPtr->ssl->session->ssl_version) {
            case SSL2_VERSION:
                string="SSLv2"; break;
            case SSL3_VERSION:
                string="SSLv3"; break;
            case TLS1_VERSION:
                string="TLSv1"; break;
            default:
                string="UNKNOWN";
        }

        Tcl_SetResult(interp, string, TCL_VOLATILE);

    } else if (STREQ(argv[1], "cipher")) {

        cipher = SSL_get_current_cipher(scPtr->ssl);

	if (STREQ(argv[2], "name")) {

	    if (argc != 3) {
		Tcl_AppendResult (interp, "wrong # args:  should be \"",
				  argv[0], argv[1], " name\"", NULL);
		status = TCL_ERROR;

	    } else {
                string = (scPtr->ssl != NULL ? (char *)SSL_CIPHER_get_name(cipher) : NULL);
                Tcl_SetResult(interp, string, TCL_VOLATILE);
	    }
        } else if (STREQ(argv[2], "strength")) { 

            if (argc != 3) {
                Tcl_AppendResult (interp, "wrong # args:  should be \"",
                                  argv[0], argv[1], " strength\"", NULL);
                status = TCL_ERROR;
            } else {
                integer = SSL_CIPHER_get_bits(cipher, &integer);
		sprintf(interp->result, "%d", integer); 
            }
        }

    } else if (STREQ(argv[1], "clientcert")) {

	clientcert = (scPtr == NULL) ? NULL : scPtr->clientcert;

	if (STREQ(argv[2], "exists")) {

	    if (argc != 3) {
		Tcl_AppendResult (interp, "wrong # args:  should be \"",
				  argv[0], argv[1], " exists\"", NULL);
		status = TCL_ERROR;

	    } else {
		Tcl_SetResult(interp, clientcert == NULL ? "0" : "1",
		    TCL_STATIC);
	    }

	} else if (STREQ(argv[2], "version")) {

	    if (argc != 3) {
		Tcl_AppendResult (interp, "wrong # args:  should be \"",
				  argv[0], argv[1], " version\"", NULL);
		status = TCL_ERROR;

	    } else {
		sprintf(interp->result, "%lu", 
		    clientcert == NULL
			? 0
			:  X509_get_version(clientcert) + 1);
	    }

	} else if (STREQ(argv[2], "serial")) {

	    if (argc != 3) {
		Tcl_AppendResult (interp, "wrong # args:  should be \"",
				  argv[0], argv[1], " serial\"", NULL);
		status = TCL_ERROR;

	    } else {
		sprintf(interp->result, "%ld",
		    clientcert == NULL
			? 0
			: ASN1_INTEGER_get(X509_get_serialNumber(clientcert)));
	    }

	} else if (STREQ(argv[2], "subject")) {
	    if (argc != 3) {
		Tcl_AppendResult (interp, "wrong # args:  should be \"",
				  argv[0], argv[1], " subject\"", NULL);
		status = TCL_ERROR;

	    } else if (clientcert != NULL) {
		SetResultToX509Name(interp, X509_get_subject_name(clientcert));
	    }

	} else if (STREQ(argv[2], "issuer")) {
	    if (argc != 3) {
		Tcl_AppendResult (interp, "wrong # args:  should be \"",
				  argv[0], argv[1], " issuer\"", NULL);
		status = TCL_ERROR;

	    } else if (clientcert != NULL) {
		SetResultToX509Name(interp, X509_get_issuer_name(clientcert));
	    }

	} else if (STREQ(argv[2], "notbefore")) {
	    if (argc != 3) {
		Tcl_AppendResult (interp, "wrong # args:  should be \"",
				  argv[0], argv[1], " notbefore\"", NULL);
		status = TCL_ERROR;

	    } else if (clientcert != NULL) {
		string = ValidTime(X509_get_notBefore(clientcert));
		if (string == NULL) {
		    Tcl_SetResult(interp, "error getting notbefore",
			TCL_STATIC);
		    status = TCL_ERROR;
		} else {
		    Tcl_SetResult(interp, string, TCL_DYNAMIC);
		}
	    }

	} else if (STREQ(argv[2], "notafter")) {
	    if (argc != 3) {
		Tcl_AppendResult (interp, "wrong # args:  should be \"",
				  argv[0], argv[1], " notafter\"", NULL);
		status = TCL_ERROR;
	    } else if (clientcert != NULL) {
		string = ValidTime(X509_get_notAfter(clientcert));
		if (string == NULL) {
		    Tcl_SetResult(interp, "error getting notafter",
			TCL_STATIC);
		    status = TCL_ERROR;
		} else {
		    Tcl_SetResult(interp, string, TCL_DYNAMIC);
		}
	    }

	} else if (STREQ(argv[2], "signature_algorithm")) {
	    if (argc != 3) {
		Tcl_AppendResult (interp, "wrong # args:  should be \"",
				  argv[0], argv[1], " signature_algorithm\"",
				  NULL);
		status = TCL_ERROR;

	    } else if (clientcert != NULL) {
		SetResultToObjectName(interp,
		    clientcert->cert_info->signature->algorithm);
	    }

	} else if (STREQ(argv[2], "key_algorithm")) {

	    if (argc != 3) {
		Tcl_AppendResult (interp, "wrong # args:  should be \"",
				  argv[0], argv[1], " key_algorithm\"", NULL);
		status = TCL_ERROR;

	    } else if (clientcert != NULL) {
		SetResultToObjectName(interp,
		    clientcert->cert_info->key->algor->algorithm);
	    }

	} else if (STREQ(argv[2], "pem")) {

	    if (argc != 3) {
		Tcl_AppendResult (interp, "wrong # args:  should be \"",
				  argv[0], argv[1], " pem\"", NULL);
		status = TCL_ERROR;

	    } else if (clientcert != NULL) {
		string = PEMCertificate(clientcert);
		if (string == NULL) {
		    Tcl_SetResult(interp, "error getting pem",
			TCL_STATIC);
		    status = TCL_ERROR;
		} else {
		    Tcl_SetResult(interp, string, TCL_DYNAMIC);
		}
	    }

	} else if (STREQ(argv[2], "valid")) {

	    if (argc != 3) {
		Tcl_AppendResult (interp, "wrong # args:  should be \"",
				  argv[0], argv[1], " valid\"", NULL);
		status = TCL_ERROR;

	    } else {
		sprintf(interp->result, "%d",
		    clientcert != NULL
		    && SSL_get_verify_result(scPtr->ssl) == X509_V_OK);
	    }

	} else {
	    Tcl_AppendResult (interp, "unknown command \"", argv[2],
		"\": should be one of: exists version serial subject issuer notbefore notafter signature_algorithm key_algorithm pem valid", NULL);
	    status = TCL_ERROR;
	}

    } else {
	Tcl_AppendResult (interp, "unknown command \"", argv[1],
	    "\": should be one of: info clientcert", NULL);
	status = TCL_ERROR;
    }

    return status;
}

/*
 *----------------------------------------------------------------------
 *
 * NsOpenSSLGetConn --
 *
 *      Return the SSL connection struct for the current connection.
 *
 * Results:
 *      NsOpenSSLConnection* or NULL.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

static NsOpenSSLConnection *
NsOpenSSLGetConn(Tcl_Interp *interp)
{
    Ns_Conn             *conn;
    NsOpenSSLConnection *cdPtr;
    char                *name;

    /* conn = Ns_GetConn();  ** Ns_GetConn is gone */
    conn = Ns_TclGetConn(interp);
    if (conn != NULL) {
	name = Ns_ConnDriverName (conn);
	if (name != NULL && STREQ (name, DRIVER_NAME)) {
	    return (NsOpenSSLConnection *) Ns_ConnDriverContext(conn);
	}
    }

    return NULL;
}

/*
 *----------------------------------------------------------------------
 *
 * SetResultToX509Name --
 *
 *      Set the Tcl interpreter's result to the string form of the
 *      specified X.509 name.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

static void
SetResultToX509Name(Tcl_Interp *interp, X509_NAME *name)
{
    char *string;

    string = X509_NAME_oneline(name, NULL, 0);
    Tcl_SetResult(interp, string, TCL_VOLATILE);
    OPENSSL_free(string);
}

/*
 *----------------------------------------------------------------------
 *
 * SetResultToObjectName --
 *
 *      Set the Tcl interpreter's result to the string form of the
 *      specified ASN.1 object name.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

static void
SetResultToObjectName(Tcl_Interp *interp, ASN1_OBJECT *obj)
{
    int   nid;
    char *string;

    nid = OBJ_obj2nid(obj);
    if (nid == NID_undef) {
	Tcl_SetResult(interp, "UNKNOWN", TCL_STATIC);
    } else {
	string = (char *) OBJ_nid2ln(nid);
	if (string == NULL) {
	    Tcl_SetResult(interp, "ERROR", TCL_STATIC);
	} else {
	    Tcl_SetResult(interp, string, TCL_VOLATILE);
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * ValidTime --
 *
 *      Takes an ASN1_UTCTIME value and converts it into a string of
 *      the form "Aug 28 20:00:38 2002 GMT"
 *
 * Results:
 *      Pointer to null-terminated string allocated by Tcl_Alloc.
 *
 * Side effects:
 *      None.
 *
 *---------------------------------------------------------------------- */

static char *
ValidTime(ASN1_UTCTIME *tm)
{
    char *result;
    BIO  *bio;
    int   n;

    if ((bio = BIO_new(BIO_s_mem())) == NULL)
	return NULL;

    ASN1_UTCTIME_print(bio, tm);
    n = BIO_pending(bio);
    result = Tcl_Alloc(n + 1);
    n = BIO_read(bio, result, n);
    result[n] = '\0';
    BIO_free(bio);

    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * PEMCertificate --
 *
 *      Retrieves the certificate in PEM format
 *
 * Results:
 *      Pointer to null-terminated string that contains the PEM
 *      certificate, allocated by Tcl_Alloc.
 *
 * Side effects:
 *      None.
 *
 *---------------------------------------------------------------------- */

static char *
PEMCertificate(X509 *clientcert)
{
    char *result;
    BIO  *bio;
    int   n;

    if ((bio = BIO_new(BIO_s_mem())) == NULL)
	return NULL;

    PEM_write_bio_X509(bio, clientcert);

    n = BIO_pending(bio);
    result = Tcl_Alloc(n + 1);
    n = BIO_read(bio, result, n);
    result[n] = '\0';
    BIO_free(bio);

    return result;
}

