To run this example.

% cd example
% (cd .. ; cabal install )
% hermit Main.hs -v0 -opt=Language.GHC.Core.Reify.Plugin +Main Main.hss
hermit<0> any-bu reify-core 

OR

% hermit Main.hs -v0 -opt=Language.GHC.Core.Reify.Plugin +Main Main.hss -ddump-if-trace        

If you want to use GHCi to test the package build.

% cd src
% ghci Language/GHC/Core/Reify/Plugin.hs  -package=ghc
