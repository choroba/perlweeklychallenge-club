#!/usr/bin/env bqn

Score ← {+´×⟜(⌽2⋆↕∘≠)⌈○(+˝)⟜¬⊏˘⊸=𝕩}

Score [[0,0,1,1],[1,0,1,0],[1,1,0,0]]
