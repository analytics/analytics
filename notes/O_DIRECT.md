Index Home About Blog
 Newsgroups: fa.linux.kernel
From: Linus Torvalds <torvalds@transmeta.com>
Subject: Re: O_DIRECT performance impact on 2.4.18 (was: Re: [PATCH] 2.5.14
Original-Message-ID: <Pine.LNX.4.44.0205100854370.2230-100000@home.transmeta.com>
Date: Fri, 10 May 2002 15:59:56 GMT
Message-ID: <fa.m6umeav.15008gs@ifi.uio.no>

On Fri, 10 May 2002, Lincoln Dale wrote:
>
> so O_DIRECT in 2.4.18 still shows up as a 55% performance hit versus no
> O_DIRECT. anyone have any clues?

Yes.

O_DIRECT isn't doing any read-ahead.

For O_DIRECT to be a win, you need to make it asynchronous.

		Linus


 Newsgroups: fa.linux.kernel
From: Linus Torvalds <torvalds@transmeta.com>
Subject: Re: O_DIRECT performance impact on 2.4.18 (was: Re: [PATCH] 2.5.14
Original-Message-ID: <Pine.LNX.4.44.0205111047280.2355-100000@home.transmeta.com>
Date: Sat, 11 May 2002 18:06:36 GMT
Message-ID: <fa.m6uaeiv.150c98r@ifi.uio.no>

On Fri, 10 May 2002, Gerrit Huizenga wrote:
> In message <Pine.LNX.4.44.0205100854370.2230-100000@home.transmeta.com>, > : Li
> nus Torvalds writes:
> >
> > For O_DIRECT to be a win, you need to make it asynchronous.
>
> O_DIRECT is especially useful for applications which maintain their
> own cache, e.g. a database.  And adding Async to it is an even bigger
> bonus (another Oracleism we did in PTX).

The thing that has always disturbed me about O_DIRECT is that the whole
interface is just stupid, and was probably designed by a deranged monkey
on some serious mind-controlling substances [*].

It's simply not very pretty, and it doesn't perform very well either
because of the bad interfaces (where synchronicity of read/write is part
of it, but the inherent page-table-walking is another issue).

I bet you could get _better_ performance more cleanly by splitting up the
actual IO generation and the "user-space mapping" thing sanely. For
example, if you want to do an O_DIRECT read into a buffer, there is no
reason why it shouldn't be done in two phases:

 (1) readahead: allocate pages, and start the IO asynchronously
 (2) mmap the file with a MAP_UNCACHED flag, which causes read-faults to
     "steal" the page from the page cache and make it private to the
     mapping on page faults.

If you split it up like that, you can do much more interesting things than
O_DIRECT can do (ie the above is inherently asynchronous - we'll wait only
for IO to complete when the page is actually faulted in).

For O_DIRECT writes, you split it the other way around:

 (1) mwrite() takes the pages in the memory area, and moves them into the
     page cache, removing the page from the page table (and only copies
     if existing pages already exist)
 (2) fdatasync_area(fd, offset, len)

Again, the above is likely to be a lot more efficient _and_ can do things
that O_DIRECT only dreams on.

With my suggested _sane_ interface, I can do a noncached file copy that
should be "perfect" even in the face of memory pressure by simply doing

	addr = mmap( ..  MAP_UNCACHED ..  src .. )
	mwrite(dst, addr, len);

which does true zero-copy (and, since mwrite removes it from the page
table anyway, you can actually avoid even the TLB overhead trivially: if
mwrite notices that the page isn't mapped, it will just take it directly
from the page cache).

Sadly, database people don't seem to have any understanding of good taste,
and various OS people end up usually just saying "Yes, Mr Oracle, I'll
open up any orifice I have for your pleasure".

			Linus

[*] In other words, it's an Oracleism.


 Newsgroups: fa.linux.kernel
From: Linus Torvalds <torvalds@transmeta.com>
Subject: Re: O_DIRECT performance impact on 2.4.18 (was: Re: [PATCH] 2.5.14
Original-Message-ID: <Pine.LNX.4.44.0205111108470.2355-100000@home.transmeta.com>
Date: Sat, 11 May 2002 18:11:49 GMT
Message-ID: <fa.m7uadiv.160g88k@ifi.uio.no>

On Sat, 11 May 2002, Alan Cox wrote:
> >
> > The thing that has always disturbed me about O_DIRECT is that the whole
> > interface is just stupid, and was probably designed by a deranged monkey
> > on some serious mind-controlling substances [*].
>
> Used with aio its extremely nice. Without the aio patches its a bit lacking
> whenever readahead is useful

But the point is that AIO is needed just to cover up the fundamental
idiocy in the interface. If the interface had been properly designed, it
would have been useful _without_ AIO.

		Linus


 Newsgroups: fa.linux.kernel
From: Larry McVoy <lm@bitmover.com>
Subject: Re: O_DIRECT performance impact on 2.4.18 (was: Re: [PATCH] 2.5.14 IDE 
	56)
Original-Message-ID: <20020511111935.B30126@work.bitmover.com>
Date: Sat, 11 May 2002 18:20:41 GMT
Message-ID: <fa.h992mfv.126ali1@ifi.uio.no>

On Sat, May 11, 2002 at 11:04:45AM -0700, Linus Torvalds wrote:
> The thing that has always disturbed me about O_DIRECT is that the whole
> interface is just stupid, and was probably designed by a deranged monkey
> on some serious mind-controlling substances [*].
>
> I bet you could get _better_ performance more cleanly by splitting up the
> actual IO generation and the "user-space mapping" thing sanely. For
> example, if you want to do an O_DIRECT read into a buffer, there is no
> reason why it shouldn't be done in two phases:

You're only halfway right.  You want to avoid the mmap altogether.  To see
why, postulate that you have infinitely fast I/O devices (I know that's
not true but it's close enough if you get enough DMA channels going at
once, it doesn't take very many to saturate memory).  For any server
application, now all your time is in the mmap().  And there is no need
for it in general, it's just there because the upper layer of the system
is too lame to handle real page frames.

Go read the splice notes, ftp://bitmover.com/pub/splice.ps because those
were written after we had tuned things enough in IRIX that it was the
VM manipulations that became the bottleneck.

Another way to think of it is this: figure out how fast the hardware could
move the data.  Now make it go that fast.  Unless you can hide all the
VM crud somehow, you won't achieve 100% of the hardware's capability.

I know I've done a bad job explaining the splice crud, but there is
some pretty cool stuff in there, if you really got it, you'd see how
the server stuff, the database stuff, the aio stuff, all I/O of any
kind can be done in terms of the splice:pull() and splice:push()
interfaces and that it is the absolute lowest cost way to have a
generic I/O layer.
--
---
Larry McVoy            	 lm at bitmover.com           http://www.bitmover.com/lm

 Newsgroups: fa.linux.kernel
From: Linus Torvalds <torvalds@transmeta.com>
Subject: Re: O_DIRECT performance impact on 2.4.18 (was: Re: [PATCH] 2.5.14
Original-Message-ID: <Pine.LNX.4.44.0205111130080.879-100000@home.transmeta.com>
Date: Sat, 11 May 2002 18:36:39 GMT
Message-ID: <fa.mjk4lcv.1d2u21r@ifi.uio.no>

On Sat, 11 May 2002, Larry McVoy wrote:
>
> You're only halfway right.  You want to avoid the mmap altogether.

See my details on doing the perfect zero-copy copy thing.

The mmap doesn't actually touch the page tables - it ends up being nothing
but a "placeholder".

So if you do

        addr = mmap( ..  MAP_UNCACHED ..  src .. )
        mwrite(dst, addr, len);

then you can think of the mmap as just a "cookie" or the "hose" between
the source and the destination.

Does it have to be an mmap? No. But the advantage of the mmap is that you
can use the mmap to modify the stream if you want to, quite transparently.
And it gives the whole thing a whole lot more flexibility, in that if you
generate the data yourself, you'd just do the mwrite() - again with zero
copy overhead.

And I personally believe that "generate the data yourself" is actually a
very common case. A pure pipe between two places is not what a computer is
good at, or what a computer should be used for.

		Linus


 Newsgroups: fa.linux.kernel
From: Linus Torvalds <torvalds@transmeta.com>
Subject: Re: O_DIRECT performance impact on 2.4.18 (was: Re: [PATCH] 2.5.14
Original-Message-ID: <Pine.LNX.4.44.0205111141070.879-100000@home.transmeta.com>
Date: Sat, 11 May 2002 18:58:09 GMT
Message-ID: <fa.mjk2lkv.1d203pq@ifi.uio.no>

On Sat, 11 May 2002, Larry McVoy wrote:
> On Sat, May 11, 2002 at 11:35:21AM -0700, Linus Torvalds wrote:
> > See my details on doing the perfect zero-copy copy thing.
> >
> > The mmap doesn't actually touch the page tables - it ends up being nothing
> > but a "placeholder".
>
> Huh, I must have missed something, does the mmap() not create any page
> tables at all?

It can. But go down to the end in my first explanation to see why it
doesn't have to.

I'll write up the implementation notes and you'll see what I'm talking
about:

 - readahead(fd, offset, size)

   Obvious (except the readahead is free to ignore the size, it's just a
   hint)

 - mmap( MAP_UNCACHED )

   This only sets up the "vma" descriptor (like all other MMAP's). It's
   exactly like a regular private mapping, except instead of just
   incrementing the page count on a page-in, it will look at whether the
   page can just be removed from the page cache and inserted as a private
   page into the mapping ("stealing" the page).

 - fdatasync_area( fd, offset, len)

   Obvious. It's fdatasync, except it only guarantees the specific range.

 - mwrite(fd, addr, len)

   This is really does the "reverse" of mmap(MAP_UNCACHED) (and like a
   mapping, addr/len have to be page-aligned).

   This walks the page tables, and does the _smart_ thing:

    - if no mapping exists, it looks at the backing store of the vma,
      and gets the page directly from the backing store instead of
      bothering to populate the page tables.

    - if the mapped page exists, it removes it from the page table

    - in either case, it moves the page it got into the page cache of the
      destination file descriptor.

NOTE on zero-copy / no-page-fault behaviour:
 - mwrite has to walk the page tables _anyway_ (the same as O_DIRECT),
   since that's the only way to do zero-copy.
 - since mwrite has to do that part, it's trivial to notice that the page
   tables don't exist. In fact, it's a very natural result of the whole
   algorithm.
 - if user space doesn't touch the mapping itself in any way (other than
   point mwrite() at it), you never build up any page tables at all, and
   you never even need to touch the TLB (ie no flushes, no nothing).
 - note how even "mmap( MAP_UNCACHED )" doesn't actually touch the TLB or
   the page tables (unless it uses MAP_FIXED and you use it to unmap a
   previous area, of course - that's all in the normal mmap code already)

See?

I will _guarantee_ that this is more efficient than any O_DIRECT ever was,
and it will get very close to your "optimal" thing (it does need to look
at some page tables, but since the page tables haven't ever really needed
to be built up for the pure copy case, it will be able to decide that the
page isn't there from the top-level page table if you align the virtual
area properly - ie at 4MB boundaries on an x86).

I suspect that this is about a few hundred lines of code (and a lot of
testing). And you can emulate O_DIRECT behaviour with it, along with
splice (only for page-cache entities, though), and a lot of other
off-by-one uses.

			Linus


 Newsgroups: fa.linux.kernel
From: Linus Torvalds <torvalds@osdl.org>
Subject: Re: statfs() / statvfs() syscall ballsup...
Original-Message-ID: <Pine.LNX.4.44.0310100839030.20420-100000@home.osdl.org>
Date: Fri, 10 Oct 2003 16:04:57 GMT
Message-ID: <fa.kh21io7.1ajck0h@ifi.uio.no>

On Fri, 10 Oct 2003, Joel Becker wrote:
> > I hope disk-based databases die off quickly.
>
> 	As opposed to what?  Not a challenge, just interested in what
> you think they should be.

I'm hoping in-memory databases will just kill off the current crop
totally. That solves all the IO problems - the only thing that goes to
disk is the log and the backups, and both go there totally linearly unless
the designer was crazy.

Yeah, I don't follow the db market, but it's just insane to try to keep
the on-disk data in any other format if you've got enough memory. Recovery
may take a long time (reading that whole backup into memory and redoing
the log will be pretty expensive), but replication should handle that
trivially.

> 	Where I work doesn't change the need for O_DIRECT.  If your Big
> App has it's own cache, why copy the cache in the kernel?

Why indeed?

But why do you think you need O_DIRECT with very bad semantics to handle
this?

The kernel page cache does multiple things:
 - staging area for letting the filesystem do blocking (ie this is why a
   regular "write()" or "read()" doesn't need to care about alignment etc)
 - a synchronization entity - making sure that a write and a read cannot
   pass each other, and that mmap contents are always _coherent_.
 - a cache

O_DIRECT throws the cache part away, but it throws out the baby with the
bathwater, and breaks the other parts. Which is why O_DIRECT breaks things
like disk scheduling in really subtle ways - think about writing and
reading to the same area on the disk, and re-ordering at all different
levels.

And the thing is, uncaching is _trivial_. It's not like it is hard to say
"try to get rid of these pages if they aren't mapped anywhere" and "insert
this user page directly into the page cache". But people are so fixated
with "direct to disk" that they don't even think about it.

			Linus


 Newsgroups: fa.linux.kernel
From: Linus Torvalds <torvalds@osdl.org>
Subject: Re: statfs() / statvfs() syscall ballsup...
Original-Message-ID: <Pine.LNX.4.44.0310100939300.20420-100000@home.osdl.org>
Date: Fri, 10 Oct 2003 16:52:54 GMT
Message-ID: <fa.kihtio7.1b38k0h@ifi.uio.no>

On Fri, 10 Oct 2003, Joel Becker wrote:
>
> 	Memory is continuously too small and too expensive.  Even if you
> can buy a machine with 10TB of RAM, the price is going to be
> prohibitive.  And when 10TB of RAM costs better, the database is going
> to be 100TB.

Hah.

Look at the number of supercomputers and the number of desktops today.

The fact is, the high end is getting smaller and smaller. If Oracle wants
to go after that high-end-only market, then be my guest.

But don't be surprised if others end up taking the remaining 99%.

Have you guys learnt _nothing_ from the past? The reason MicroSoft and
Linux are kicking all the other vendors butts is that _small_ is
beautiful. Especially when small is "powerful enough".

Hint: why does Oracle care at all about the small business market? Why is
MySQL even a blip on your radar? Because it's those things that really
_drive_ stuff. The same way PC's have driven the tech market for the last
15 years.

And believing that the load will keep up with "big iron hardware" is just
not _true_. It's never been true. "Small iron" not only keeps up, but
overtakes it - to the point where you have to start doing new things just
to be able to take advantage of it.

Believe in history.

>
> > O_DIRECT throws the cache part away, but it throws out the baby with the
> > bathwater, and breaks the other parts. Which is why O_DIRECT breaks things
> > like disk scheduling in really subtle ways - think about writing and
> > reading to the same area on the disk, and re-ordering at all different
> > levels.
>
> 	Sure, but you don't do that.  The breakage in mixing O_DIRECT
> with pagecache I/O to the same areas of the disk isn't even all that
> subtle.  But you shouldn't be doing that, at least constantly.

Ok. Let's just hope all the crackers and virus writers believe you when
you say "you shouldn't do that".

BIG FRIGGING HINT: a _real_ OS doesn't allow data corruption even for
cases where "you shouldn't do that". It shouldn't allow reading of data
that you haven't written. And "you shouldn't do that" is _not_ an excuse
for having bad interfaces that cause problems.

We're not NT.

		Linus

 Newsgroups: fa.linux.kernel
From: Linus Torvalds <torvalds@osdl.org>
Subject: Re: statfs() / statvfs() syscall ballsup...
Original-Message-ID: <Pine.LNX.4.44.0310101046060.20420-100000@home.osdl.org>
Date: Fri, 10 Oct 2003 17:52:57 GMT
Message-ID: <fa.khhnj04.1a3mlou@ifi.uio.no>

On Fri, 10 Oct 2003, Joel Becker wrote:
>
> 	I know that, I agree with it, and I said as much a few emails
> past.  Linux should refuse to corrupt your data.  But you've taken the
> tack "It is unsafe today, so we should abandon it altogether, never mind
> fixing it.", which doesn't logically follow.

No, we've fixed it, the problem is that it ends up being a lot of extra
complexity that isn't obvious when just initially looking at it. For
example, just the IO scheduler ended up having serious problems with
overlapping IO requests. That's in addition to all the issues with
out-of-sync ordering etc that could cause direct_io reads to bypass
regular writes and read stuff off the disk that was a potential security
issue.

So right now we have extra code and extra complexity (which implies not
only potential for more bugs, but there are performance worries etc that
can impact even users that don't need it).

And these are fundamental problems to DIRECT_IO. Which means that likely
at some point we will _have_ to actually implement DIRECT_IO entirely
through the page cache to make sure that it's safe. So my bet is that
eventually we'll make DIRECT_IO just be an awkward way to do page cache
manipulation.

And maybe it works out ok. And we'll clearly have to keep it working. The
issue is whether there are better interfaces. And I think there are bound
to be.

		Linus


 Newsgroups: fa.linux.kernel
From: Linus Torvalds <torvalds@osdl.org>
Subject: Re: statfs() / statvfs() syscall ballsup...
Original-Message-ID: <Pine.LNX.4.44.0310100958030.20420-100000@home.osdl.org>
Date: Fri, 10 Oct 2003 17:06:46 GMT
Message-ID: <fa.kh23j86.1ajelgg@ifi.uio.no>

On Fri, 10 Oct 2003, Chris Friesen wrote:
>
> How does this play with massive (ie hundreds or thousands of gigabytes)
> databases?  Surely you can't expect to put it all in memory?

Hey, I'm a big believer in mass market.

Which means that I think odd-ball users will have to use odd-ball
databases, and pay through the nose for them. That's fine. But those db's
are doing to be very rare.

Your arguments are all the same stuff that made PC's "irrelevant" 15 years
ago.

I'm not saying in-memory is here tomorrow. I'm just saying that anybody
who isn't looking at it for the mass market _will_ be steamrolled over
when they arrive.

If you were a company, which market would you prefer: the high-end 0.1% or
the rest? Yes, you can charge a _lot_ more for the high-end side, but you
will eternally live in the knowledge that your customers are slowly moving
to the "low end" - simply because it gets more capable.

And the thing is, the economics of the 99% means that that is the one that
sees all the real improvements. That's the one that will have the nice
admin tools, and the cottage industry that builds up around it.

			Linus


 Newsgroups: fa.linux.kernel
From: Linus Torvalds <torvalds@osdl.org>
Subject: Re: statfs() / statvfs() syscall ballsup...
Original-Message-ID: <Pine.LNX.4.44.0310101006310.20420-100000@home.osdl.org>
Date: Fri, 10 Oct 2003 17:09:12 GMT
Message-ID: <fa.kj1di04.1bjokou@ifi.uio.no>

On Fri, 10 Oct 2003, Linus Torvalds wrote:
>
> I'm not sayign in-memory is here tomorrow. I'm just saying that anybody
> who isn't looking at it for the mass market _will_ be steamrolled over
> when they arrive.

Btw, anybody that takes me too seriously is an idiot. I know what _I_
believe in, but part of the beauty of Linux is that what I believe doesn't
really matter all that much.

			Linus


 Newsgroups: fa.linux.kernel
From: Linus Torvalds <torvalds@osdl.org>
Subject: Re: statfs() / statvfs() syscall ballsup...
Original-Message-ID: <Pine.LNX.4.44.0310101024200.20420-100000@home.osdl.org>
Date: Fri, 10 Oct 2003 17:42:56 GMT
Message-ID: <fa.kihbig2.1b3qk8s@ifi.uio.no>

On Fri, 10 Oct 2003, Joel Becker wrote:
>
> 	msync() forces write(), like fsync().  It doesn't force read().

Actually, the kernel has a "readahead(fd, offset, size)" system call that
will start asynchronous read-ahead on any mapping. After that, just
touching the page will obviously map in and synchronize the result.

I don't think anybody uses it, and the interface may be broken, but it was
literally 20 lines of code, and I had a trivial test program that
populated the cache for a directory structure really quickly using it.

In general, it would be really nice to have more oracle people discussing
what their particular pet horror is, and what they'd really like to do.

I know you're more used to just doing your own thing and working with
vendors, but even just people getting used to do the unofficial "this is
what we do, and it sucks because xxx" would make people more aware of what
you want to do, and maybe it would suggest novel ways of doing things.

I suspect most of the things would get shot down as being impractical, but
there have always been a lot of discussion about more direct control of
the page cache for programs that really want it, and I'm more than willing
to discuss things (obviously 2.7.x material, but still.. A lot of it is
trivial and could be back-ported to 2.6.x if people start using it).

For example, things we can do, but don't, partly because of interface
issues and because there is no point in doing it if people wouldn't use
it:

 - moving a page back and forth between user space. It's _trivial_ to do,
   with a fallback on copying if the page happens to be busy (ie we can
   often just replace the existing page cache page, but if somebody else
   has it mapped, we'd have to copy the contents instead)

   We can't do this for "regular" read and write, because the resulting
   copy-on-write sitution makes it less than desireable in most cases, but
   if the user space specifically says "you can throw these pages away
   after moving them to the page cache", that avoids a lot of horror.

   The "remap_file_pages()" thing kind of does this on the read side (ie
   it says "map in this page cache entry into my virtual address space"),
   but we don't have the reverse aka "take this page in the virtual
   address space and map it into the page cache".

   Interfaces like these would also allow things like zero-copy file
   copies with smaller page cache footprints - at the expense of
   invalidating the cache for the source file as a result of the copy.
   Which is why it can't be a _regular_ read - but it's one of those
   things where if the user knows what he wants..

 - dirty mapping control (ie controlling partial page dirty state, and
   also _delaying_ writeout if it needs to be ordered). Possibly by having
   a separate backing store (ie a mmap that says "read from this file, but
   write back to that other file") to avoid the nasty memory management
   problems.

A lot of these are really easy to do, but the usage and the interfaces are
non-obvious.

		Linus


 Newsgroups: fa.linux.kernel
From: Linus Torvalds <torvalds@osdl.org>
Subject: Re: statfs() / statvfs() syscall ballsup...
Original-Message-ID: <Pine.LNX.4.44.0310101059330.20420-100000@home.osdl.org>
Date: Fri, 10 Oct 2003 18:06:29 GMT
Message-ID: <fa.kj1hj87.1bjslgh@ifi.uio.no>

On Fri, 10 Oct 2003, Trond Myklebust wrote:
>
> Apart from O_DIRECT, we have nothing in the kernel as it stands that
> will allow userland to deal with this case.

Oh, but that's just another case of the general notion of allowing people
to control the page cache a bit more.

There's nothing wrong with having kernel interfaces that say "this region
is potentially stale" or "this region is dirty" or "this region is not
needed any more".

For example, using DIRECT_IO to make sure that something is uptodate is
just _stupid_, because clearly it only matters to shared-disk (either over
networks/FC or though things like SCSI device sharing) setups. So now the
app has to have a way to query for whether the storage is shared, and
have two totally different code-paths depending on the answer.

This is another example of a bad design, that ends up causing more
problems (remember why this thread started in the first place: bad design
of O_DIRECT causing the app to have to care about something _else_ it
shouldn't care about. At all).

If you had a "this region is stale" thing, you'd just use it. And if it
was local disk, it wouldn't do anything.

		Linus


 Newsgroups: fa.linux.kernel
From: Linus Torvalds <torvalds@osdl.org>
Subject: Re: statfs() / statvfs() syscall ballsup...
Original-Message-ID: <Pine.LNX.4.44.0310101402370.25501-100000@home.osdl.org>
Date: Fri, 10 Oct 2003 21:10:14 GMT
Message-ID: <fa.kjibi7u.1b3mkgo@ifi.uio.no>

On Fri, 10 Oct 2003, Trond Myklebust wrote:
>
> In fact, I recently noticed that we still have this race in the NFS
> file locking code: readahead may have been scheduled before we
> actually set the file lock on the server, and may thus fill the page
> cache with stale data.

The current "invalidate_inode_pages()" is _not_ equivalent to a specific
user saying "these pages are bad and have to be updated".

The main difference is that invalidate_inode_pages() really cannot assume
that the pages are bad: the pages may be mapped into another process that
is actively writing to them, so the regular "invalidate_inode_pages()"
literally must not force a re-read - that would throw out real
information.

So "invalidate_inode_pages()" really is a hint, not a forced eviction.

A forced eviction can be done only by a user that says "I have write
permission to this file, and I will now say that these pages _have_ to be
thrown away, whether dirty or not".

And that's totally different, and will require a totally different
approach.

(As to the read-ahead issue: there's nothing saying that you can't wait
for the pages if they aren't up-to-date, and really synchronize with
read-ahead. But that will require filesystem help, if only to be able to
recognize that there is active IO going on. So NFS would have to keep
track of a "read list" the same way it does for writeback pages).

		Linus


 Newsgroups: fa.linux.kernel
From: Linus Torvalds <torvalds@osdl.org>
Subject: Re: statfs() / statvfs() syscall ballsup...
Original-Message-ID: <Pine.LNX.4.44.0310101126120.20420-100000@home.osdl.org>
Date: Fri, 10 Oct 2003 18:38:02 GMT
Message-ID: <fa.ki1hig4.1ajsk8u@ifi.uio.no>

On Fri, 10 Oct 2003, Andrea Arcangeli wrote:
>
> O_DIRECT only walk the pagetables, no pte mangling, no tlb flushes, the
> TLB is preserved fully.

Yes. However, it's even _nicer_ if you don't need to walk the page tables
at all.

Quite a lot of operations could be done directly on the page cache. I'm
not a huge fan of mmap() myself - the biggest advantage of mmap is when
you don't know your access patterns, and you have reasonably good
locality. In many other cases mmap is just a total loss, because the page
table walking is often more expensive than even a memcpy().

That's _especially_ true if you have to move mappings around, and you have
to invalidate TLB's.

memcpy() often gets a bad name. Yeah, memory is slow, but especially if
you copy something you just worked on, you're actually often better off
letting the CPU cache do its job, rather than walking page tables and
trying to be clever.

Just as an example: copying often means that you don't need nearly as much
locking and synchronization - which in turn avoids one whole big mess
(yes, the memcpy() will look very hot in profiles, but then doing extra
work to avoid the memcpy() will cause spread-out overhead that is a lot
worse and harder to think about).

This is why a simple read()/write() loop often _beats_ mmap approaches.
And often it's actually better to not even have big buffers (ie the old
"avoid system calls by aggregation" approach) because that just blows your
cache away.

Right now, the fastest way to copy a file is apparently by doing lots of
~8kB read/write pairs (that data may be slightly stale, but it was true at
some point). Never mind the system call overhead - just having the extra
buffer stay in the L1 cache and avoiding page faults from mmap is a bigger
win.

And I don't think mmap _can_ beat that. It's fundamental.

In contrast, direct page cache accesses really can do so. Exactly because
they don't touch any page tables at all, and because they can take
advantage of internal kernel data structure layout and move pages around
without any cost..

		Linus


 Newsgroups: fa.linux.kernel
From: Linus Torvalds <torvalds@osdl.org>
Subject: Re: statfs() / statvfs() syscall ballsup...
Original-Message-ID: <Pine.LNX.4.44.0310120909050.12190-100000@home.osdl.org>
Date: Sun, 12 Oct 2003 16:15:26 GMT
Message-ID: <fa.kgibh8g.1b36lgo@ifi.uio.no>

On 12 Oct 2003, Greg Stark wrote:
>
> There are other reasons databases want to control their own cache. The
> application knows more about the usage and the future usage of the data than
> the kernel does.

But this again is not an argument for not using the page cache - it's only
an argument for _telling_ the kernel about its use.

> However on busy servers whenever it's run it causes lots of pain because the
> kernel flushes all the cached data in favour of the data this job touches.

Yes. But this is actually pretty easy to avoid in-kernel, since all of the
LRU logic is pretty localized.

It could be done on a per-process thing ("this process should not pollute
the active list") or on a per-fd thing ("accesses through this particular
open are not to pollute the active list").

>									 And
> worse, there's no way to indicate that the i/o it's doing is lower priority,
> so i/o bound servers get hit dramatically.

IO priorities are pretty much worthless. It doesn't _matter_ if other
processes get preferred treatment - what is costly is the latency cost of
seeking. What you want is not priorities, but batching.

			Linus

 From: Linus Torvalds <torvalds@osdl.org>
Newsgroups: fa.linux.kernel
Subject: Re: O_DIRECT question
Date: Thu, 11 Jan 2007 03:07:02 UTC
Message-ID: <fa.5XV/mpvjizQdvJcA7I/TP8ld9o8@ifi.uio.no>

On Thu, 11 Jan 2007, Aubrey wrote:
>
> Now, my question is, is there a existing way to mount a filesystem
> with O_DIRECT flag? so that I don't need to change anything in my
> system. If there is no option so far, What is the right way to achieve
> my purpose?

The right way to do it is to just not use O_DIRECT.

The whole notion of "direct IO" is totally braindamaged. Just say no.

	This is your brain: O
	This is your brain on O_DIRECT: .

	Any questions?

I should have fought back harder. There really is no valid reason for EVER
using O_DIRECT. You need a buffer whatever IO you do, and it might as well
be the page cache. There are better ways to control the page cache than
play games and think that a page cache isn't necessary.

So don't use O_DIRECT. Use things like madvise() and posix_fadvise()
instead.

		Linus

 From: Linus Torvalds <torvalds@osdl.org>
Newsgroups: fa.linux.kernel
Subject: Re: O_DIRECT question
Date: Thu, 11 Jan 2007 03:17:08 UTC
Message-ID: <fa.pPS8KL8hy0jvS94bo3+w/g7XatI@ifi.uio.no>

On Wed, 10 Jan 2007, Linus Torvalds wrote:
>
> So don't use O_DIRECT. Use things like madvise() and posix_fadvise()
> instead.

Side note: the only reason O_DIRECT exists is because database people are
too used to it, because other OS's haven't had enough taste to tell them
to do it right, so they've historically hacked their OS to get out of the
way.

As a result, our madvise and/or posix_fadvise interfaces may not be all
that strong, because people sadly don't use them that much. It's a sad
example of a totally broken interface (O_DIRECT) resulting in better
interfaces not getting used, and then not getting as much development
effort put into them.

So O_DIRECT not only is a total disaster from a design standpoint (just
look at all the crap it results in), it also indirectly has hurt better
interfaces. For example, POSIX_FADV_NOREUSE (which _could_ be a useful and
clean interface to make sure we don't pollute memory unnecessarily with
cached pages after they are all done) ends up being a no-op ;/

Sad. And it's one of those self-fulfilling prophecies. Still, I hope some
day we can just rip the damn disaster out.

			Linus

 From: Linus Torvalds <torvalds@osdl.org>
Newsgroups: fa.linux.kernel
Subject: Re: O_DIRECT question
Date: Thu, 11 Jan 2007 15:51:41 UTC
Message-ID: <fa.K0FGO9lUeTUnkr4y/tXDi0UcgBM@ifi.uio.no>

On Thu, 11 Jan 2007, Nick Piggin wrote:
>
> Speaking of which, why did we obsolete raw devices? And/or why not just
> go with a minimal O_DIRECT on block device support? Not a rhetorical
> question -- I wasn't involved in the discussions when they happened, so
> I would be interested.

Lots of people want to put their databases in a file. Partitions really
weren't nearly flexible enough. So the whole raw device or O_DIRECT just
to the block device thing isn't really helping any.

> O_DIRECT is still crazily racy versus pagecache operations.

Yes. O_DIRECT is really fundamentally broken. There's just no way to fix
it sanely. Except by teaching people not to use it, and making the normal
paths fast enough (and that _includes_ doing things like dropping caches
more aggressively, but it probably would include more work on the device
queue merging stuff etc etc).

The "good" news is that CPU really is outperforming disk more and more, so
the extra cost of managing the page cache keeps on getting smaller and
smaller, and (fingers crossed) some day we can hopefully just drop
O_DIRECT and nobody will care.

		Linus

 From: Linus Torvalds <torvalds@osdl.org>
Newsgroups: fa.linux.kernel
Subject: Re: O_DIRECT question
Date: Thu, 11 Jan 2007 17:06:03 UTC
Message-ID: <fa.8KjfRgjr3QZUdPAg4r64CZsTiSk@ifi.uio.no>

On Thu, 11 Jan 2007, Xavier Bestel wrote:

> Le jeudi 11 janvier 2007 à 07:50 -0800, Linus Torvalds a écrit :
> > > O_DIRECT is still crazily racy versus pagecache operations.
> >
> > Yes. O_DIRECT is really fundamentally broken. There's just no way to fix
> > it sanely.
>
> How about aliasing O_DIRECT to POSIX_FADV_NOREUSE (sortof) ?

That is what I think some users could do. If the main issue with O_DIRECT
is the page cache allocations, if we instead had better (read: "any")
support for POSIX_FADV_NOREUSE, one class of reasons O_DIRECT usage would
just go away.

See also the patch that Roy Huang posted about another approach to the
same problem: just limiting page cache usage explicitly.

That's not the _only_ issue with O_DIRECT, though. It's one big one, but
people like to think that the memory copy makes a difference when you do
IO too (I think it's likely pretty debatable in real life, but I'm totally
certain you can benchmark it, probably even pretty easily especially if
you have fairly studly IO capabilities and a CPU that isn't quite as
studly).

So POSIX_FADV_NOREUSE kind of support is one _part_ of the O_DIRECT
picture, and depending on your problems (in this case, the embedded world)
it may even be the *biggest* part. But it's not the whole picture.

		Linus

 From: Linus Torvalds <torvalds@osdl.org>
Newsgroups: fa.linux.kernel
Subject: Re: O_DIRECT question
Date: Thu, 11 Jan 2007 19:02:02 UTC
Message-ID: <fa.LPPoVm3n2QeAkd0qfvTonGXNp8E@ifi.uio.no>

On Thu, 11 Jan 2007, Trond Myklebust wrote:
>
> For NFS, the main feature of interest when it comes to O_DIRECT is
> strictly uncached I/O. Replacing it with POSIX_FADV_NOREUSE won't help
> because it can't guarantee that the page will be thrown out of the page
> cache before some second process tries to read it. That is particularly
> true if some dopey third party process has mmapped the file.

You'd still be MUCH better off using the page cache, and just forcing the
IO (but _with_ all the page cache synchronization still active). Which is
trivial to do on the filesystem level, especially for something like NFS.

If you bypass the page cache, you just make that "dopey third party
process" problem worse. You now _guarantee_ that there are aliases with
different data.

Of course, with NFS, the _server_ will resolve any aliases anyway, so at
least you don't get file corruption, but you can get some really strange
things (like the write of one process actually happening before, but being
flushed _after_ and overriding the later write of the O_DIRECT process).

And sure, the filesystem can have its own alias avoidance too (by just
probing the page cache all the time), but the fundamental fact remains:
the problem is that O_DIRECT as a page-cache-bypassing mechanism is
BROKEN.

If you have issues with caching (but still have to allow it for other
things), the way to fix them is not to make uncached accesses, it's to
force the cache to be serialized. That's very fundamentally true.

		Linus

 From: Linus Torvalds <torvalds@osdl.org>
Newsgroups: fa.linux.kernel
Subject: Re: O_DIRECT question
Date: Thu, 11 Jan 2007 16:46:06 UTC
Message-ID: <fa.8p4rOgK3VgXYz1hsJZ//X4KnaFM@ifi.uio.no>

On Thu, 11 Jan 2007, Roy Huang wrote:
>
> On a embedded systerm, limiting page cache can relieve memory
> fragmentation. There is a patch against 2.6.19, which limit every
> opened file page cache and total pagecache. When the limit reach, it
> will release the page cache overrun the limit.

I do think that something like this is probably a good idea, even on
non-embedded setups. We historically couldn't do this, because mapped
pages were too damn hard to remove, but that's obviously not much of a
problem any more.

However, the page-cache limit should NOT be some compile-time constant. It
should work the same way the "dirty page" limit works, and probably just
default to "feel free to use 90% of memory for page cache".

		Linus

 From: Linus Torvalds <torvalds@osdl.org>
Newsgroups: fa.linux.kernel
Subject: Re: O_DIRECT question
Date: Fri, 12 Jan 2007 04:47:07 UTC
Message-ID: <fa.GolGdeUXCoTXpT6yDZ59cZjOiaI@ifi.uio.no>

On Fri, 12 Jan 2007, Nick Piggin wrote:
>
> We are talking about about fragmentation. And limiting pagecache to try to
> avoid fragmentation is a bandaid, especially when the problem can be solved
> (not just papered over, but solved) in userspace.

It's not clear that the problem _can_ be solved in user space.

It's easy enough to say "never allocate more than a page". But it's often
not REALISTIC.

Very basic issue: the perfect is the enemy of the good. Claiming that
there is a "proper solution" is usually a total red herring. Quite often
there isn't, and the "paper over" is actually not papering over, it's
quite possibly the best solution there is.

		Linus

 From: Linus Torvalds <torvalds@osdl.org>
Newsgroups: fa.linux.kernel
Subject: Re: O_DIRECT question
Date: Fri, 12 Jan 2007 05:19:52 UTC
Message-ID: <fa.2PGYOGa6tKFYNKOaiclDev2+faM@ifi.uio.no>

On Fri, 12 Jan 2007, Nick Piggin wrote:
>
> Yeah *smallish* higher order allocations are fine, and we use them all the
> time for things like stacks or networking.
>
> But Aubrey (who somehow got removed from the cc list) wants to do order 9
> allocations from userspace in his nommu environment. I'm just trying to be
> realistic when I say that this isn't going to be robust and a userspace
> solution is needed.

I do agree that order-9 allocations simply is unlikely to work without
some pre-allocation notion or some serious work at active de-fragmentation
(and the page cache is likely to be the _least_ of the problems people
will hit - slab and other kernel allocations are likely to be much much
harder to handle, since you can't free them in quite as directed a
manner).

But for smallish-order (eg perhaps 3-4 possibly even more if you are
careful in other places), the page cache limiter may well be a "good
enough" solution in practice, especially if other allocations can be
controlled by strict usage patterns (which is not realistic in a general-
purpose kind of situation, but might be realistic in embedded).

		Linus

 From: Linus Torvalds <torvalds@osdl.org>
Newsgroups: fa.linux.kernel
Subject: Re: O_DIRECT question
Date: Thu, 11 Jan 2007 16:21:28 UTC
Message-ID: <fa.Xcr3OG0EVHvJ04SJvclIJzO+8eg@ifi.uio.no>

On Thu, 11 Jan 2007, Viktor wrote:
>
> OK, madvise() used with mmap'ed file allows to have reads from a file
> with zero-copy between kernel/user buffers and don't pollute cache
> memory unnecessarily. But how about writes? How is to do zero-copy
> writes to a file and don't pollute cache memory without using O_DIRECT?
> Do I miss the appropriate interface?

mmap()+msync() can do that too.

Also, regular user-space page-aligned data could easily just be moved into
the page cache. We actually have a lot of the infrastructure for it. See
the "splice()" system call. It's just not very widely used, and the
"drop-behind" behaviour (to then release the data) isn't there. And I bet
that there's lots of work needed to make it work well in practice, but
from a conceptual standpoint the O_DIRECT method really is just about the
*worst* way to do things.

O_DIRECT is "simple" in the sense that it basically is a "OS: please just
get out of the way" method. It's why database people like it, and it's why
it has gotten implemented in many operating systems: it *looks* like a
simple interface.

But deep down, O_DIRECT is anything but simple. Trying to do a direct
access with an interface that really isn't designed for it (write()
_fundamentally_ has semantics that do not fit the problem in that you're
supposed to be able to re-use the buffer immediately afterwards in user
space, just as an example) is wrong in the first place, but the really
subtle problems come when you realize that you can't really just "bypass"
the OS.

As a very concrete example: people *think* that they can just bypass the
OS IO layers and just do the write directly. It *sounds* like something
simple and obvious. It sounds like a total no-brainer. Which is exactly
what it is, if by "no-brainer" you mean "only a person without a brain
will do it". Because by-passing the OS has all these subtle effects on
both security and on fundamental correctness.

The whole _point_ of an OS is to be a "resource manager", to make sure
that people cannot walk all over each other, and to be the central point
that makes sure that different people doing allocations and deallocations
don't get confused. In the specific case of a filesystem, it's "trivial"
things like serializing IO, allocating new blocks on the disk, and making
sure that nobody will see the half-way state when the dirty blocks haven't
been written out yet.

O_DIRECT - by bypassing the "real" kernel - very fundamentally breaks the
whole _point_ of the kernel. There's tons of races where an O_DIRECT user
(or other users that expect to see the O_DIRECT data) will now see the
wrong data - including seeing uninitialized portions of the disk etc etc.

In short, the whole "let's bypass the OS" notion is just fundamentally
broken. It sounds simple, but it sounds simple only to an idiot who writes
databases and doesn't even UNDERSTAND what an OS is meant to do. For some
reasons, db people think that they don't need one, and don't ever seem to
really understand the concept of "security" and "correctness". They
understand it (sometimes) _within_ their own database, but seem to have a
really hard time seeing past their own sandbox.

Some of the O_DIRECT breakage could probably be fixed:

 - An O_DIRECT operation must never allocate new blocks on the disk. It's
   fundamentally broken. If you *cannot* write new blocks, and can only
   read and re-write previous allocations, things are much easier, and a
   lot of the races go away.

   This is probably _perfectly_ fine for the users (namely databases).
   People who do O_DIRECT really expect to see a "raw disk image", but
   they (exactly _because_ they expect a raw disk image) are perfectly
   happy to "set up" that image beforehand.

 - An O_DIRECT operation must never race with any metadata operation,
   most notably truncate(), but also any file extension operation like a
   normal write() that extends the size of the file.

   This should be reasonably easy to do. Any O_DIRECT operation would just
   take the inode->i_mutex for reading. HOWEVER. Right now it's a mutex,
   not a read-write semaphore, so that is actually pretty painful. But it
   would be fairly simple.

With those two rules, a lot of the complexity of the nasty side effects of
O_DIRECT that the db people obviously never even thought about would go
away. We'd still have to have some way to synchronize the page cache, but
it could be as simple as having an O_DIRECT open simply _flush_ the whole
page cache, and set some flag saying "can't do normal opens, we're
exclusively open for O_DIRECT".

I dunno. A lot of filesystems don't want to (or can't) actually do a
"write in place" ANYWAY (writes happen through the log, and hit the "real
filesystem" part of the disk later), and O_DIRECT really only makes sense
if you do the write in place, so the above rules would help make that
obvious too - O_DIRECT really is a totally different thing from a normal
IO, and an O_DIRECT write() or read() really has *nothing* to do with a
regular write() or read() system call.

Overloading a totally different operation with a flag is a bad idea, which
is one reason I really hate O_DIRECT. It's just doing things badly on so
many levels.

			Linus

 From: Linus Torvalds <torvalds@osdl.org>
Newsgroups: fa.linux.kernel
Subject: Re: O_DIRECT question
Date: Thu, 11 Jan 2007 18:01:34 UTC
Message-ID: <fa.iAxlPoPry99+YXZFaVLfVfbxb7I@ifi.uio.no>

On Thu, 11 Jan 2007, Alan wrote:
>
> Well you can - its called SG_IO and that really does get the OS out of
> the way. O_DIRECT gets crazy when you stop using it on devices directly
> and use it on files

Well, on a raw disk, O_DIRECT is fine too, but yeah, you might as well
use SG_IO at that point. All of my issues are all about filesystems.

And filesystems is where people use O_DIRECT most. Almost nobody puts
their database on a partition of its own these days, afaik. Perhaps for
benchmarking or some really high-end stuff. Not "normal users".

		Linus

 From: Linus Torvalds <torvalds@osdl.org>
Newsgroups: fa.linux.kernel
Subject: Re: O_DIRECT question
Date: Fri, 12 Jan 2007 18:08:00 UTC
Message-ID: <fa.ccSWk7m98C5zEWIGHkMLV0ZR1qw@ifi.uio.no>

On Thu, 11 Jan 2007, dean gaudet wrote:
>
> it seems to me that if splice and fadvise and related things are
> sufficient for userland to take care of things "properly" then O_DIRECT
> could be changed into splice/fadvise calls either by a library or in the
> kernel directly...

The problem is two-fold:

 - the fact that databases use O_DIRECT and all the commercial people are
   perfectly happy to use a totally idiotic interface (and they don't care
   about the problems) means that things like fadvise() don't actually
   get the TLC. For example, the USEONCE thing isn't actually
   _implemented_, even though from a design standpoint, it would in many
   ways be preferable over O_DIRECT.

   It's not just fadvise. It's a general problem for any new interfaces
   where the old interfaces "just work" - never mind if they are nasty.
   And O_DIRECT isn't actually all that nasty for users (although the
   alignment restrictions are obviously irritating, but they are mostly
   fundamental _hardware_ alignment restrictions, so..). It's only nasty
   from a kernel internal security/serialization standpoint.

   So in many ways, apps don't want to change, because they don't really
   see the problems.

   (And, as seen in this thread: uses like NFS don't see the problems
   either, because there the serialization is done entirely somewhere
   *else*, so the NFS people don't even understand why the whole interface
   sucks in the first place)

 - a lot of the reasons for problems for O_DIRECT is the semantics. If we
   could easily implement the O_DIRECT semantics using something else, we
   would. But it's semantically not allowed to steal the user page, and it
   has to wait for it to be all done with, because those are the semantics
   of "write()".

   So one of the advantages of vmsplice() and friends is literally that it
   could allow page stealing, and allow the semantics where any changes to
   the page (in user space) might make it to disk _after_ vmsplice() has
   actually already returned, because we literally re-use the page (ie
   it's fundamentally an async interface).

But again, fadvise and vmsplice etc aren't even getting the attention,
because right now they are only used by small programs (and generally not
done by people who also work on the kernel, and can see that it really
would be better to use more natural interfaces).

> looking at the splice(2) api it seems like it'll be difficult to implement
> O_DIRECT pread/pwrite from userland using splice... so there'd need to be
> some help there.

You'd use vmsplice() to put the write buffers into kernel space (user
space sees it's a pipe file descriptor, but you should just ignore that:
it's really just a kernel buffer). And then splice the resulting kernel
buffers to the destination.

		Linus

 From: Linus Torvalds <torvalds@osdl.org>
Newsgroups: fa.linux.kernel
Subject: Re: O_DIRECT question
Date: Fri, 12 Jan 2007 21:19:22 UTC
Message-ID: <fa.DEhvgqIrW8EkkrThtz/HB1ce6A4@ifi.uio.no>

On Sat, 13 Jan 2007, Michael Tokarev wrote:
>
> (No, really - this load isn't entirely synthetic.  It's a typical database
> workload - random I/O all over, on a large file.  If it can, it combines
> several I/Os into one, by requesting more than a single block at a time,
> but overall it is random.)

My point is that you can get basically ALL THE SAME GOOD BEHAVIOUR without
having all the BAD behaviour that O_DIRECT adds.

For example, just the requirement that O_DIRECT can never create a file
mapping, and can never interact with ftruncate would actually make
O_DIRECT a lot more palatable to me. Together with just the requirement
that an O_DIRECT open would literally disallow any non-O_DIRECT accesses,
and flush the page cache entirely, would make all the aliases go away.

At that point, O_DIRECT would be a way of saying "we're going to do
uncached accesses to this pre-allocated file". Which is a half-way
sensible thing to do.

But what O_DIRECT does right now is _not_ really sensible, and the
O_DIRECT propeller-heads seem to have some problem even admitting that
there _is_ a problem, because they don't care.

A lot of DB people seem to simply not care about security or anything
else.anything else. I'm trying to tell you that quoting numbers is
pointless, when simply the CORRECTNESS of O_DIRECT is very much in doubt.

I can calculate PI to a billion decimal places in my head in .1 seconds.
If you don't care about the CORRECTNESS of the result, that is.

See? It's not about performance. It's about O_DIRECT being fundamentally
broken as it behaves right now.

		Linus

 From: Linus Torvalds <torvalds@osdl.org>
Newsgroups: fa.linux.kernel
Subject: Re: O_DIRECT question
Date: Fri, 12 Jan 2007 22:12:20 UTC
Message-ID: <fa.ZIxcCFFuOGQMDFMrxjZjf40G5U8@ifi.uio.no>

On Sat, 13 Jan 2007, Michael Tokarev wrote:
> >
> > At that point, O_DIRECT would be a way of saying "we're going to do
> > uncached accesses to this pre-allocated file". Which is a half-way
> > sensible thing to do.
>
> Half-way?

I suspect a lot of people actually have other reasons to avoid caches.

For example, the reason to do O_DIRECT may well not be that you want to
avoid caching per se, but simply because you want to limit page cache
activity. In which case O_DIRECT "works", but it's really the wrong thing
to do. We could export other ways to do what people ACTUALLY want, that
doesn't have the downsides.

For example, the page cache is absolutely required if you want to mmap.
There's no way you can do O_DIRECT and mmap at the same time and expect
any kind of sane behaviour. It may not be what a DB wants to use, but it's
an example of where O_DIRECT really falls down.

> > But what O_DIRECT does right now is _not_ really sensible, and the
> > O_DIRECT propeller-heads seem to have some problem even admitting that
> > there _is_ a problem, because they don't care.
>
> Well.  In fact, there's NO problems to admit.
>
> Yes, yes, yes yes - when you think about it from a general point of
> view, and think how non-O_DIRECT and O_DIRECT access fits together,
> it's a complete mess, and you're 100% right it's a mess.

You can't admit that even O_DIRECT _without_ any non-O_DIRECT actually
fails in many ways right now.

I've already mentioned ftruncate and block allocation. You don't seem to
understand that those are ALSO a problem.

				Linus
