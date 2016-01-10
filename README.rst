=================
README.AlbOrgMode
=================

:Precis: Emacs lisp to configure Org-Mode for albcorp
:Authors: Andrew Lincoln Burrow
:Contact: albcorp@gmail.com
:Copyright: 2010-2012, 2015-2016 Andrew Lincoln Burrow

--------
Overview
--------

*AlbOrgMode* is a collection of emacs lisp to configure Org-Mode for
albcorp.  It contains examples of emacs lisp functions to customise
Org-Mode, and a complete customisation of Org-Mode for keybindings.  In
particular, it provides an implementation of Getting Things Done (GTD_).

.. _GTD:
   http://gettingthingsdone.com/

Status
======

This software has been developed for personal use.  While it should
provide useful examples for customising Org-Mode, it is not recommended
as a drop in configuration, since it bakes in my own key binding and
configuration.  I hope to generalise the configuration in this
repository over time.  I expect to use the issue tracker.

------------
Installation
------------

To install *AlbOrgMode* simply place the contents of the repository in a
convenient location.  The source code is installed once Emacs can locate
the emacs lisp files.  These cases are covered in the subsection on
minimal installation.  Further configuration is suggested in the
subsection on additional configuration.

Minimal installation
====================

To install *AlbOrgMode* simply place the contents of the repository in a
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

The minimal installation of *AlbOrgMode* does not impose any new
configuration on Org-Mode.  This section briefly describes how to
configure Org-Mode to make use of the features in *AlbOrgMode*.

Load the *AlbOrgMode* key bindings by adding the following fragment to
``~/.emacs``

::
   (add-hook 'org-mode-hook `alb-org-key-bindings)

.. Local Variables:
.. mode: rst
.. ispell-local-dictionary: "british"
.. End:
