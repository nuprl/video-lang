#lang scribble/manual

@title{Artifact: Super8, the Language for Making Movies---A Functional Pearl}}
@(author (author+email "Leif Andersen" "leif@ccs.neu.edu")
         (author+email "Stephen Chang" "stchang@ccs.neu.edu")
         (author+email "Matthias Felleisen" "matthias@ccs.neu.edu"))

This is the README file for the artifact that accompanies:
``Super8, the Language for Making Movies---A Functional Pearl'' in ICFP 2017

Our artifact consists of a VM image that contains:
@itemlist[
 @item{a distribution of the Racket programming language (version 6.9),}
 @item{a distribution of the Video programming language,}
 @item{the required libraries to support Video's renderer,}
 @item{example programs written in Video,}
 @item{the source code for this paper,}
 @item{and a distribution of the Typed Video variant of Video.}]

The goals of this are to:
@itemlist[
 @item{provide a simple environment to create and run Video programs and}
 @item{provide an archival copy of Video.}]

Note that at the time of creation for this VM, the video language can be found at:
@url["https://github.com/LeifAndersen/racket-video"]

@section{Setting up and installing the artifact}

The artifact is available as a virtual machine appliance for VirtualBox. If
you are already reading this README in the VM, feel free to ignore the
rest of this section.

To run the artifact image, open the given @tt{.ovf} file using the
@tt{File->Import Appliance} menu item. This will create a new VM
that can be launched after import. We recommend giving the VM at least
4GB of RAM.

The image is configured to automatically login to the @tt{artifact} user
account. The account has root privileges using @tt{sudo} without a password.
The password for the account is @tt{artifact}.

@section{Artifact Overview}

The relevant files for this artifact are in
@filepath["/home/artifact/Desktop"], which contains:
@itemlist[
 @item{@filepath{README.html}--this file,}
 @item{@filepath{paper/}--the source code for the paper,}
 @item{@filepath{video/}--the copy of Video,}
 @item{@filepath{typed-video/}--the copy of Typed Video,}
 @item{@filepath{examples/}--example Video programs,}
 @item{@filepath{DrRacket}--an IDE for editing Video programs,}
 @item{and @filepath{run.rkt}--a script to run the examples in this repo.}]

@section{Walkthrough}

We conclude by giving a walkthrough on how to create and run
some simple Video programs.
