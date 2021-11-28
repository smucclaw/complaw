The whole Github complaw project has been imported into Overleaf.

The Github <-> Overleaf integration has lots of rough edges, in particular:
Overleaf is not capable of resolving merge conflicts. Overleaf is also
incapable to rewind to a previous version in the history (one can inspect
elements of the history, but make no reset).

The integration is OK for a basic interaction:

* For pulling the Github repo into Overleaf: In Overleaf, click on the "Menu"
  button in the upper left corner; then click on "Github", then "Pull Github
  changes into Overleaf". The synchronisation takes ages. The message "No
  Github changes since last merge" is misleading.
* Similarly for pushing from Overleaf to Github. The author of the commit will
  always be the owner of the Overleaf project (in this case: Meng).

To compile a subproject (e.g.: LACompLing2021): In Overleaf, click on the
"Menu" button in the upper left corner; in the Settings, choose "Main
document", for example lacompling2021.tex; return to the main window and
Recompile.
