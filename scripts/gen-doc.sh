#!/bin/sh

set -e

lein with-profile +gen-doc codox
