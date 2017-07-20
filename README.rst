===========================
README for ``alb-org-mode``
===========================

:Precis: Emacs lisp to configure Org-Mode for albcorp
:Authors: Andrew Lincoln Burrow
:Contact: albcorp@gmail.com
:Copyright: 2010-2012, 2015-2017 Andrew Lincoln Burrow

------------
Introduction
------------

*alb-org-mode* is a collection of emacs lisp to configure Org-Mode for
albcorp.  It contains examples of emacs lisp functions to customise
Org-Mode, and a complete customisation of Org-Mode for keybindings.  In
particular, it provides an implementation of Getting Things Done (GTD_).

This software has been developed for personal use.  However, it will be
of value to others making extensive use of Org-Mode in the following
ways.

* It provides functions that can be used within the configuration
  exposed by `org-customise`.  The functions extend heading sorting,
  note capture, and agenda and column views.  These are useful examples
  for others attempting to similar customisation.  Future updates will
  provide examples of using these functions and commentary on their
  design
* It implements GTD_ in Org-Mode.  There are many decisions to be made
  in mapping your own work processes to Org-Mode.  The implementation
  reflects my experience using Org-Mode for over 5 years.  Future
  updates will provide examples of this implementation and commentary on
  its design
* It radically streamlines the Org-Mode key bindings.  This reflects my
  principal frustration with Org-Mode: it favours features over user
  experience; it is over stuffed with features, many of which cannot
  repay the cost in learning how they work.  In contrast, the most
  critical processes in maintaining and reviewing project notes are
  poorly supported by Org-Mode.  provides an extremely utility of these
  functions and should provide useful examples for customising Org-Mode,
  and is in the process of being refactored to become both a collection
  of resuable configurationallow for a collection of resources to not
  recommended as a drop in configuration, since it bakes in my own key
  bindings and configuration.  I hope to generalise the configuration in
  this repository over time.

I expect to use the issue tracker.

The remainder of this README is organised as follows:

- `Installation`_ sets out the minimal steps for installation.
  Installation makes the functions provided in this repository available
  to your own Org-Mode configuration, but does not alter your Org-Mode
  configuration

.. _GTD:
   http://gettingthingsdone.com/

------------
Installation
------------

To install *alb-org-mode* simply place the contents of the repository in
a convenient location.  The source code is installed once Emacs can
locate the emacs lisp files.  These cases are covered in the subsection
on minimal installation.  Installation makes the functions provided in
this repository available to your own Org-Mode configuration, but does
not alter your Org-Mode configuration.  Further configuration is
suggested in the subsection on additional configuration.

Minimal installation
====================

To install *alb-org-mode* simply place the contents of the repository in a
convenient location.  The source code is installed once Emacs can locate
the emacs lisp files.  The repository contains helper scripts to achieve
these ends.

``alb-org-mode-start.el``
  This emacs lisp code can be sourced from your personal emacs init
  file.  It updates the load path, and sets hooks to load the key map.

  Again, one approach is to place the repository under the directory
  ``~/Config`` and add the following fragment to ``~/.emacs``

  ::

      ;;
      ;;
      ;; SCRIPTED CONFIGURATION
      ;; ---------------------------------------------------------------
      ;;

      (mapcar 'load
              (file-expand-wildcards "~/Config/*/*-start.el" t))

  Change the filename ``${HOME}/Config`` to suit your personal
  directory structure.

Configuration
=============

The minimal installation of *alb-org-mode* does not impose any new
configuration on Org-Mode.  This section briefly describes how to
configure Org-Mode to make use of the features in *alb-org-mode*.

Load the *alb-org-mode* key bindings by adding the following fragment to
``~/.emacs``

::
   (add-hook 'org-mode-hook `alb-org-key-bindings)

------------
Key bindings
------------

.. Local Variables:
.. mode: rst
.. ispell-local-dictionary: "british"
.. End:
