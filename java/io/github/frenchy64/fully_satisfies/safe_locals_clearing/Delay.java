/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Jun 28, 2007 */

package io.github.frenchy64.fully_satisfies.safe_locals_clearing;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import clojure.lang.IDeref;
import clojure.lang.IFn;
import clojure.lang.IPending;
import clojure.lang.Util;

public class Delay implements IDeref, IPending{
Object val;
Throwable exception;
IFn fn;
volatile ReentrantLock lock;

public Delay(IFn f){
	fn = f;
	val = null;
	exception = null;
	lock = new ReentrantLock();
}

static public Object force(Object x) {
	return (x instanceof Delay) ?
	       ((Delay) x).deref()
	       : x;
}

private void realize() {
	ReentrantLock l = lock;
	if(l != null) {
    // bail if recurring - Ambrose
    if(l.isHeldByCurrentThread()) {
      throw Util.sneakyThrow(Util.runtimeException("Recursive delay dereference"));
    }
		l.lock();
		try {
			if(fn!=null) {
				try {
					val = fn.invoke();
				} catch (Throwable t) {
					exception = t;
				}
				fn = null;
				lock = null;
			}
		} finally {
			l.unlock();
		}
	}
}

public Object deref() {
	if(lock != null)
		realize();
	if(exception != null)
		throw Util.sneakyThrow(exception);
	return val;
}

public boolean isRealized(){
	return lock == null;
}
}
