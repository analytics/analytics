# Short Term Goals:

* Task Management
 * Refactor `Data.Analytics.Task` to something closer to `Control.Monad.Par.Meta` from `meta-par`, while retaining `dynamicWind`.
 * Implement an affinity scheduler.

* Buffer Management
 * Implement a couple of buffer management schemes. e.g.
     * Exploit GHC's amazing I/O manager and the nature of cache oblivious algorithms with the dumbest thing that could possibly work.
       Just `mmap` anything requested and work with it.
     * `O_DIRECT` with `aio`-based buffered execution and some blocking-resource management layer in `Task`.

* Obliviousness
 * Implement Blelloch et al's low-depth sorting algorithm using `Task` for work-stealing.
 * Implement APMA and PDPMA as primitive building blocks.
