SIGSIGS=GENERAL LIST BOOL STRING SUBSTRING CHAR \
 VECTOR ARRAY ARRAY2 DATE TIME TIMER WORD BYTE REAL MATH RANDOM OPTION LISTSORT

SMLSIGS=INTEGER STRING_CVT MONO_VECTOR MONO_ARRAY MONO_VECTOR_SLICE MONO_ARRAY_SLICE \
 VECTOR_SLICE ARRAY_SLICE INT_INF PACK_WORD LIST_PAIR

IMPL=General List Bool Int31 Int32 IntInf String2 \
 Substring ByteTable Char Array2 Array Vector VectorSlice \
 ArraySlice ByteSlice Date Time Timer Word8 Word31 Word32 \
 Byte Real Math Random Pack32Little Pack32Big ListPair Option Listsort \
 StringCvt ConcatString

THESIGS=$(SMLSIGS:%=../../../js/basis/%.sml) $(SIGSIGS:%=../../../js/basis/%.sig)
THEIMPL=$(IMPL:%=../../../js/basis/%.sml)

sigdoc: sigdoc.mlb main.sml Makefile
	mlkit -o sigdoc sigdoc.mlb

doc: sigdoc
	./sigdoc -sigs $(THESIGS) -impl $(THEIMPL)

clean:
	rm -rf *~ MLB run sigdoc *.html
