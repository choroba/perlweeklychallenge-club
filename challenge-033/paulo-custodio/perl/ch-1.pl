#!/usr/bin/perl

# Challenge 033
#
# Task #1
# Count Letters (A..Z)
# Create a script that accepts one or more files specified on the command-line
# and count the number of times letters appeared in the files.
#
# So with the following input file sample.txt
#
# The quick brown fox jumps over the lazy dog.
# the script would display something like:
#
# a: 1
# b: 1
# c: 1
# d: 1
# e: 3
# f: 1
# g: 1
# h: 2
# i: 1
# j: 1
# k: 1
# l: 1
# m: 1
# n: 1
# o: 4
# p: 1
# q: 1
# r: 2
# s: 1
# t: 2
# u: 2
# v: 1
# w: 1
# x: 1
# y: 1
# z: 1

use Modern::Perl;

# collect
my %count;
while (<>) {
    for (split //, lc($_)) {
        if (/[a-z]/) {
            $count{$_}++;
        }
    }
}

# output
for (sort keys %count) {
    say "$_: $count{$_}";
}
