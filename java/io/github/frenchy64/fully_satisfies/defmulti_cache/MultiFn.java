/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Sep 13, 2007 */

package io.github.frenchy64.fully_satisfies.defmulti_cache;

import java.util.Map;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import clojure.lang.*;

public class MultiFn extends clojure.lang.MultiFn{
public MultiFn(String name, IFn dispatchFn, Object defaultDispatchVal, IRef hierarchy) {
  super(name, dispatchFn, defaultDispatchVal, hierarchy);
}
}
