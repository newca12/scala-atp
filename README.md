# scala-atp [![Build Status](https://travis-ci.org/newca12/scala-atp.svg?branch=master)](https://travis-ci.org/newca12/scala-atp) [![Coverage Status](https://coveralls.io/repos/newca12/scala-atp/badge.png)](https://coveralls.io/r/newca12/scala-atp) [![Ohloh](http://www.openhub.net/p/scala-atp/widgets/project_thin_badge.gif)](https://www.openhub.net/p/scala-atp)

### About ###
Port of the Objective Caml code supporting John Harrison's logic [textbook](http://www.cambridge.org/catalogue/catalogue.asp?isbn=9780521899574) Handbook of Practical Logic and Automated Reasoning to Scala.

scala-atp is an EDLA project.

The purpose of [edla.org](https://edla.org) is to promote the state of the art in various domains.

### Other ports ###
[F#](https://github.com/jack-pappas/fsharp-logic-examples) : complete and 'official'  
[SML](https://github.com/logic-tools/sml-handbook): almost complete and 'official'  
[Haskell](https://github.com/seereason/atp-haskell) : complete and [published](https://hackage.haskell.org/package/atp-haskell)  
[OCaml](https://github.com/newca12/ocaml-atp) : original code for Ocam 3 adapted to fit OCaml 4  
[Haskell](https://github.com/newca12/haskell-atp) : complete, original code for GHC 6.10.4 adapted to fit GHC 8.x  
[Haskell](https://github.com/etu-fkti5301-bgu/alt-exam_automated_theorem_proving) : almost complete  
[JavaScript](https://github.com/etu-fkti5301-bgu/alt-exam_atp_system) : almost complete (generated with GHCJS compiler)  
[scala](https://github.com/inpefess/practical-logic-handbook) : uncomplete (chap 1 & 2)  
[Haskell](https://github.com/relrod/HPLAR) : uncomplete (chap 1)  
[Haskell](https://github.com/elliottt/plar) : uncomplete  
[Rust](https://github.com/nikomatsakis/plar-rs) : uncomplete  
[Swift](https://github.com/nikomatsakis/hplar) : uncomplete

### Running examples with [Jupyter notebook](http://jupyter.org/) ###

All examples will be available in the [notebooks directory](https://github.com/newca12/scala-atp/tree/master/notebooks)  
This is a very convenient way to play with Scala  
You need first to publish scala-atp locally :
```
https://github.com/newca12/scala-atp.git
cd scala-atp
sbt publishLocal
```
Follow instructions [here](https://github.com/jupyter-scala/jupyter-scala) to install a scala kernel for Jupyter.  
To run the notebook, run the following command at the Terminal 
```
jupyter-notebook
```
### Notes ###
We kept the file names of the original mostly intact, though use Scala naming conventions.  
This project contain also a study about parsers and some code from :  
* [DSLs in Action](http://books.google.fr/books?id=SzD6RAAACAAJ&num=16&source=gbs_slider_cls_metadata_1003_mylibrary) from Debasish Ghosh  
* Eugen Labun's thesis : [Combinator Parsing in Scala](https://docs.google.com/file/d/0B7LbY7bJaltldVRhMHhfT1F4VkE/edit?pli=1)

### License ###
© 2003-2007, John Harrison.   
© 2012-2019, Olivier ROLAND.  
(See "LICENSE.txt" for details.)

