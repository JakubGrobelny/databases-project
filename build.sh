cabal sandbox init;
cabal update;
cabal install --only-dependencies;
cabal build;
ln -s dist/build/databases-project/databases-project exe