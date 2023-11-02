A few months ago, I bought a home server, with the intention of
de-cloud-ing as much as I could manage, as well as setting up some
nice services for around the house. Things like a media server for my
digital library, or a home setup for tracking various DVCS repos and
associated CI, but without having to be tied to either making things
public on github, putting up with Atlassian's... shenanigans', or
forking over cash for the ability to make a private repo.

Cloud services are well and good, and even in my most radical dreams,
I suspect I'll always have a few online services (at least email, but
ProtonMail makes that convenient and privacy-oriented).

Some other requirements: I want this to run on Gentoo, and I want to
run a setup that does *not* use SystemD.

Gentoo, partly for nostalgia (I ran a gentoo as my daily linux distro
driver before college, and have fond memories of it), partly because I
finally have the compute to run a from-source distro easily, and reap
the speed benefits of compiling for my specific CPU. As for choosing
OpenRC over SystemD, I really prefer the modular/"do one thing, do it
well"/keep things mechanically simple as much as possible approach of
Linux circa the 70s to late 90s.

There's also some reason to speculate at the [underlying
motives](https://unixsheikh.com/articles/the-real-motivation-behind-systemd.html)
behind the maintainers of SystemD, which others have covered at
length.

So far, I've gotten Jellyfin and NextCloud up and running, and
storing/serving data off a dual 20TB ZFS mirror, which has been quite
lovely. I've had partial success with Sandstorm, which looks very
appealing, but has a few quirks about it.

First and foremost, it _really_ expects you to be installing this from
a server with a public IP. My home server does not have such, by
choice -- I'm planning on accessing it via OpenVPN or WireGuard when
I'm not at home, and having it sit behind my router on the local
network, otherwise. Second, having a certificate for SSL traffic is
rather non-negotiable, as well as having a wildcard DNS entry for the
domain the server will eventually sit on.

Well, after a lot of educational, trial-and-error footwork with both
bind and OpenSSL, I have those things. I've also imported the signing
certificate public key to my laptop's Firefox install, so it doesn't
bleet at me saying that someone has compromised the domain. I also set
up my router to refer to the afore-mentioned bind/named instance, so I
can just type in my server's name into Firefox and be on my merry.

So far, so good. All this snaps into place for Jellyfin, NextCloud,
and Sandstorm.

Except, something-or-other is going wrong with _some_ of the grains in
Sandstorm.

I can run an instance of Gogs (a git web UI) with no problem. Same
with FileDrop. Occasionally I'll have to hard-refresh if the grain is
loading up for the first time in a session, but after that it's smooth
sailing.

Gitlab, Etherpad, or ShareLaTeX, on the other hand, stall and refuse
to successfully load, no matter how long you wait.

Traditional wisdom among the Sandstorm wise say that I've configured
my wildcard host URL, DNS wildcard, or self-signed cert incorrectly,
but as far as I can tell, all of this is set up and working correctly.

So, I'm working on getting that up and running. Overall, however, I'm
really happy with the setup. I love having a ZFS mirror set up, and
having NextCloud upload to it, and JellyFin manage libraries from it
is quite elegant. I need to make both run as the same user (or add
each to the other's group ID) so that uploaded media can be managed by
JellyFin.

Gentoo has been, overall, a blast to use. Packages aren't quite as
ubiquitous as they are on Debian or Red Hat based distros, but I love
having this level of fine-grained control over what fetaures are
enabled, and how packages are compiled has been a blast.

Working with OpenRC has also been a blast. My next to-do on that front
is to get process monitoring set up (there's a
[guide](https://wiki.gentoo.org/wiki/OpenRC/supervise-daemon) for it,
I just need to follow it). It's been very pleasant to work with
something... simple, again.

In any case. Next up is getting GitLab up and running, either natively
on the Gentoo box, or by debugging whatever's going on with Sandstorm.

If this sort of thing appeals to you, I highly encourage you to tinker
with it. It's my opinion that the internet was better when it was
owner-operated, and I'm hoping things go back to that model in the
future.
