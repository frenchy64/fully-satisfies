/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 25, 2006 4:28:27 PM */

package io.github.frenchy64.fully_satisfies.lazier;

import clojure.lang.AFn;
import clojure.lang.ArrayChunk;
import clojure.lang.ChunkedCons;
import clojure.lang.ISeq;
import clojure.lang.LazySeq;
import java.util.Iterator;

public class RT{

////////////// Collections support /////////////////////////////////

private static final int CHUNK_SIZE = 32;
public static ISeq chunkIteratorSeq(final Iterator iter){
    if(iter.hasNext()) {
        return new LazySeq(new AFn() {
            public Object invoke() {
                Object[] arr = new Object[CHUNK_SIZE];
                int n = 0;
                while(n < CHUNK_SIZE && iter.hasNext()) // switch conditions to avoid realizing extra element - Ambrose
                    arr[n++] = iter.next();
                return new ChunkedCons(new ArrayChunk(arr, 0, n),
                    // call chunkIteratorSequence to avoid forcing extra item - Ambrose
                    chunkIteratorSequence(iter));
            }
        });
    }
    return null;
}

public static ISeq chunkIteratorSequence(final Iterator iter){
    // return a LazySeq instead of calling hasNext() and forcing first element - Ambrose
    return new LazySeq(new AFn() {
        public Object invoke() {
            Object[] arr = new Object[CHUNK_SIZE];
            int n = 0;
            // switched condition order so hasNext() isn't called 33 times - Ambrose
            while(n < CHUNK_SIZE && iter.hasNext())
                arr[n++] = iter.next();
            return new ChunkedCons(new ArrayChunk(arr, 0, n), chunkIteratorSequence(iter));
        }
    });
}
}
