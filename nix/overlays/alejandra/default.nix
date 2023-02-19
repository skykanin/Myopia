final: prev: {
  alejandra = prev.alejandra.overrideAttrs (pFinal: pPrev: {
    # Removes annoying ads in CLI
    patches = [./remove-ads.patch];
  });
}
