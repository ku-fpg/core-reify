To run this example.

% hermit Main.hs -v0 -opt=Language.GHC.Core.Reify.Plugin +Main Main.hss
hermit<0> any-bu reify-core 
        

If you want to use GHCi,

% cd src
% ghci Language/GHC/Core/Reify/Plugin.hs  -package=ghc
