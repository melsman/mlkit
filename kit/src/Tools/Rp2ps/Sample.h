#ifndef _SAMPLE_H
#define _SAMPLE_H

#define THRESHOLD_PERCENT 0 /* Threshold used when sorting samples out in SortIdentTable. */

/*------------------------------------*
 * External functions.                *
 *------------------------------------*/

void MakeIdentTable();
void allocNewSample(int sampleNo, float sampleTime);
void storeSampleEntry(int sampleNo, float sampleTime, char* id, float nbytes);
void addComment(float commentTime, char *comment);
void addMark(float markTime);
void printIdentTable(void);
void SortIdentTable(void);
void addComment(float commentTime, char *comment);
/*

Here we show some of the datastructures used.

sampleTable, table holding the sample time for each sample.
                                         +-----nsamples
                                         |
                                        \ /
     +--------------------------+--------------------------+--------------------------+
     | sampleTime               | sampleTime               | sampleTime               |
     +--------------------------+--------------------------+--------------------------+

markTable, table holding times were marks have to be inserted.
                                         +-----nmarks
                                         |
                                        \ /
     +--------------------------+--------------------------+--------------------------+
     | markTime                 | markTime                 | markTime                 |
     +--------------------------+--------------------------+--------------------------+

commentTable, table holding times were comments have to be inserted.
                                         +-----ncomments
                                         |
                                        \ /
     +--------------------------+--------------------------+--------------------------+
     | commentTime              | commentTime              | commentTime              |
     +--------------------------+--------------------------+--------------------------+

commentString, table holding pointers to strings that have to be printed at time commentTime.
                                         +-----ncomments
                                         |
                                        \ /
     +--------------------------+--------------------------+--------------------------+
     | commentString            | commentString            | commentString            |
     +--------------------------+--------------------------+--------------------------+

identTable, table holding pointers to idents, that is ENTRYs.
                                         +-----nidents
                                         |
                                        \ /
     +--------------------------+--------------------------+--------------------------+
     | ENTRYPTR                 | ENTRYPTR                 | ENTRYPTR                 |
     +--------------------------+--------------------------+--------------------------+
This table is sorted by SortIdentTable and Deviation respectively.


hashTable, table holding pointers to idents, that is ENTRYs.

     +--------------------------+--------------------------+--------------------------+
     | ENTRYPTR----+            | ENTRYPTR--------+        | ENTRYPTR                 |
     +-------------|------------+-----------------|--------+--------------------------+
                   |                              |
                  \ /                            \ /
              +---------+                    +---------+                        
              | name----+->"RegionId23i"     | name----+->"RegionId45f"         
              | next----+----ENTRYPTR------> | next----+-----ENTRYPTR------->     
              | samples |		     | samples |                        
              +----|----+		     +----|----+                        
                   |			          |                             
                   |SAMPLEPTR		          |SAMPLEPTR                    
                   |			          |                             
                   \/			          \/                            
              +-------------+		     +-------------+                    
              | n=sampleNo  |		     | n=sampleNo  |                    
	      | t=samleTime |		     | t=samleTime |                    
              | nbytes      |		     | nbytes      |                    
              | next--|     |		     | next--|     |                    
              +-------|-----+		     +-------|-----+                    
                      |SAMPLEPTR	             |SAMPLEPTR                 
                      |			             |                          
                      \/                             \/

Global variables:
  nidents holds the number of identifiers.
  nsamples holds the number of samples (incremented in allocNewSample). 
  ncomments holds the number of comments.
  nmarks holds the number of marks.
*/

#endif /* _SAMPLE_H */
