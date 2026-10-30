SIGSIGS=GENERAL LIST BOOL STRING SUBSTRING CHAR \
 VECTOR ARRAY ARRAY2 DATE TIME TIMER WORD BYTE REAL IEEE_REAL MATH RANDOM OPTION LISTSORT \
 JS HTML RWP XMLRPC XMLRPC_TYPE dojo utest

SMLSIGS=INTEGER STRING_CVT MONO_VECTOR MONO_ARRAY MONO_VECTOR_SLICE MONO_ARRAY_SLICE \
 VECTOR_SLICE ARRAY_SLICE INT_INF MONO_ARRAY2 PACK_REAL PACK_WORD LIST_PAIR JS_CORE

IMPL=General List Bool Int31 Int32 IntInf String2 \
 Substring ByteTable Char Array2 Array Vector VectorSlice \
 ArraySlice ByteSlice Date Time Timer Word8 Word31 Word32 \
 Byte Real Math IEEEReal Math16 Real16 Math32 Real32 real64tables realtables \
 PackReal16 PackReal32 Random Pack32Little Pack32Big ListPair Option Listsort \
 StringCvt JsCore Js Html Rwp XMLrpcClient dojo utest

THESIGS=$(SMLSIGS:%=../../js/basis/%.sml) $(SIGSIGS:%=../../js/basis/%.sig)
THEIMPL=$(IMPL:%=../../js/basis/%.sml)

.PHONY: doc
doc: Makefile
	make -C ../../src/Tools/SigDoc sigdoc
	./../../src/Tools/SigDoc/sigdoc -sigs $(THESIGS) -impl $(THEIMPL)

.PHONY: clean
clean:
	rm -rf *~ *.html generated_tags.js
