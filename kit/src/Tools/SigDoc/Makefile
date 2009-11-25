SIGS=GENERAL LIST BOOL INTEGER STRING SUBSTRING STRING_CVT CHAR MONO_VECTOR \
 MONO_ARRAY VECTOR ARRAY MONO_VECTOR_SLICE MONO_ARRAY_SLICE \
 VECTOR_SLICE ARRAY_SLICE ARRAY2 INT_INF DATE TIME TIMER WORD \
 BYTE REAL MATH RANDOM PACK_WORD LIST_PAIR OPTION LISTSORT

IMPL=General List Bool Int31 Int32 IntInf String2 \
 Substring ByteTable Char Array2 Array Vector VectorSlice \
 ArraySlice ByteSlice Date Time Timer Word8 Word31 Word32 \
 Byte Real Math Random Pack32Little Pack32Big ListPair Option Listsort \
 StringCvt ConcatString

THESIGS=$(SIGS:%=../../../js/basis/%.sml)
THEIMPL=$(IMPL:%=../../../js/basis/%.sml)

sigdoc: sigdoc.mlb main.sml Makefile
	mlkit -o sigdoc sigdoc.mlb

doc: sigdoc
	./sigdoc -sigs $(THESIGS) -impl $(THEIMPL)

clean:
	rm -rf *~ MLB run sigdoc *.html
