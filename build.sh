cabal update;
cabal sandbox init;
cabal install --only-dependencies;
cabal build;
ln -s dist/build/databases-project/databases-project exe