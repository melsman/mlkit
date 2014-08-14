SIGSIGS=GENERAL LIST BOOL STRING SUBSTRING CHAR \
 VECTOR ARRAY ARRAY2 DATE TIME TIMER WORD BYTE REAL MATH RANDOM OPTION LISTSORT \
 JS HTML RWP XMLRPC XMLRPC_TYPE dojo

SMLSIGS=INTEGER STRING_CVT MONO_VECTOR MONO_ARRAY MONO_VECTOR_SLICE MONO_ARRAY_SLICE \
 VECTOR_SLICE ARRAY_SLICE INT_INF PACK_WORD LIST_PAIR JS_CORE

IMPL=General List Bool Int31 Int32 IntInf String2 \
 Substring ByteTable Char Array2 Array Vector VectorSlice \
 ArraySlice ByteSlice Date Time Timer Word8 Word31 Word32 \
 Byte Real Math Random Pack32Little Pack32Big ListPair Option Listsort \
 StringCvt JsCore Js Html Rwp XMLrpcClient dojo

THESIGS=$(SMLSIGS:%=../../js/basis/%.sml) $(SIGSIGS:%=../../js/basis/%.sig)
THEIMPL=$(IMPL:%=../../js/basis/%.sml)

.PHONY: doc
doc:
	make -C ../../src/Tools/SigDoc sigdoc
	./../../src/Tools/SigDoc/sigdoc -sigs $(THESIGS) -impl $(THEIMPL)

.PHONY: clean
clean:
	rm -rf *~ *.html generated_tags.js
