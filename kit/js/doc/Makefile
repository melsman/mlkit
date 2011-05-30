SIGS=GENERAL LIST BOOL INTEGER STRING SUBSTRING STRING_CVT CHAR MONO_VECTOR \
 MONO_ARRAY VECTOR ARRAY MONO_VECTOR_SLICE MONO_ARRAY_SLICE \
 VECTOR_SLICE ARRAY_SLICE ARRAY2 INT_INF DATE TIME TIMER WORD \
 BYTE REAL MATH RANDOM PACK_WORD LIST_PAIR OPTION LISTSORT \
 JS_CORE JS HTML RWP

IMPL=General List Bool Int31 Int32 IntInf String2 \
 Substring ByteTable Char Array2 Array Vector VectorSlice \
 ArraySlice ByteSlice Date Time Timer Word8 Word31 Word32 \
 Byte Real Math Random Pack32Little Pack32Big ListPair Option Listsort \
 StringCvt JsCore Js Html Rwp XMLrpcClient

THESIGS=$(SIGS:%=../../js/basis/%.sml) ../../js/basis/XMLRPC.sig ../../js/basis/XMLRPC_TYPE.sig
THEIMPL=$(IMPL:%=../../js/basis/%.sml)

.PHONY: doc
doc:
	make -C ../../src/Tools/SigDoc sigdoc
	./../../src/Tools/SigDoc/sigdoc -sigs $(THESIGS) -impl $(THEIMPL)

.PHONY: clean
clean:
	rm -rf *~ *.html
