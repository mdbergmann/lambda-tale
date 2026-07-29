;;; Lambda's Tale — package definition.

(defpackage :tale
  (:use :common-lisp)
  (:export
   ;; directions
   #:+north+ #:+east+ #:+south+ #:+west+
   #:dir-index #:dir-keyword #:dir-opposite #:turn-dir
   ;; dice
   #:*rng* #:roll #:parse-dice #:roll-dice
   #:dice-range #:dice-range-text
   ;; ILBM images
   #:make-image #:read-ilbm #:write-ilbm #:pixel-ref
   #:image-width #:image-height #:image-depth #:image-palette #:image-pixels
   #:image-transparent-p #:mask-bytes
   #:image-frame-file #:image-frame-files
   ;; map model
   #:parse-map #:load-map-file #:map-viewport
   #:dungeon-map-name #:dungeon-map-width #:dungeon-map-height
   #:dungeon-map-wrap #:dungeon-map-start-x #:dungeon-map-start-y
   #:dungeon-map-start-facing #:dungeon-map-kind #:dungeon-map-title
   #:dungeon-map-dark #:map-title
   #:dungeon-map-encounters #:dungeon-map-encounter-chance
   #:dungeon-map-night-encounters #:dungeon-map-night-encounter-chance
   #:dungeon-map-idle-encounter-minutes
   #:cell-wall #:cell-feature #:cell-special #:wall-passable-p #:neighbor
   ;; knowledge
   #:make-map-knowledge #:know-cell #:know-wall #:know-found
   #:cell-explored-p #:cell-found-p #:wall-known-p
   ;; first-person view geometry
   #:+view-depth+ #:*draw-depth* #:+view-flanks+ #:*draw-flanks*
   #:compute-view #:view-planes
   #:view-display-list
   #:view-blit-list #:wall-piece-rect #:wall-piece-names #:wall-piece-file
   #:wall-piece-variant-file
   #:backdrop-rects #:print-tile-manifest
   #:view-slice-depth #:view-slice-cx #:view-slice-cy
   #:view-slice-front #:view-slice-left #:view-slice-right
   #:view-slice-lx #:view-slice-ly #:view-slice-left-front
   #:view-slice-rx #:view-slice-ry #:view-slice-right-front
   #:view-slice-left-fronts #:view-slice-right-fronts
   #:compass-points
   ;; game state / movement
   #:new-game #:game-map #:game-knowledge #:game-x #:game-y #:game-facing
   #:game-party #:game-flags #:game-combat
   #:game-effects #:add-effect #:remove-effect #:find-effect
   #:effect-name #:effect-expires-at #:effect-payload #:effect-label
   #:effect-image #:effect-image-path #:apply-effect-spec
   #:*timed-effect-keys* #:*instant-effect-keys*
   #:check-effect-spec #:effect-spec-combat-only-p
   #:effect-spec-target-kind
   ;; an effect spec read back out in player's words (the cards)
   #:*effect-phrases* #:effect-summary-lines #:effect-duration-text
   #:effects-ac-bonus #:effects-damage-bonus #:effects-save-bonus
   #:effects-extra-attacks #:effects-foes-ac #:effects-foes-attack
   #:effects-regen-sp #:effects-combat-heal
   #:light-active-p #:compass-active-p #:levitate-active-p
   #:turn-left #:turn-right #:turn-around #:move-party #:observe
   ;; game time, day and night
   #:game-time #:advance-time #:daylight-p #:clock-line
   #:game-dark-p #:game-view-depth #:render-view-depth
   #:time-of-day #:game-time-of-day #:time-of-day-name #:time-of-day-line
   #:sky-color-for #:ground-color-for
   #:*idle-clock-rate* #:idle-minutes-elapsed #:idle-minutes-cost
   #:*moonlight-depth*
   #:*minutes-per-action* #:*new-game-minutes* #:+minutes-per-day+
   ;; the world: zones and travel
   #:game-zones #:travel-party #:load-campaign
   ;; events and story flags
   #:on-event #:emit #:say #:flag #:set-flag #:clear-flag
   ;; sound: 8SVX samples and the cue layer (see src/sound.lisp)
   #:make-sound #:sound-rate #:sound-bytes #:read-8svx #:write-8svx
   #:*sound-names* #:*sound-backend* #:play-sound #:attach-sounds
   #:zone-sfx-dir #:load-sound-pack #:dungeon-map-sfx
   ;; message log (the Bard's Tale text column)
   #:attach-message-log #:log-message #:log-recent
   #:log-length #:log-since #:expire-messages #:*message-ttl*
   #:wrap-text #:wrap-message
   ;; structured menu lines (option rows carry their pick key)
   #:menu-option #:menu-numbered #:menu-next-option
   #:menu-line-text #:menu-line-key
   #:menu-texts #:wrap-menu-line #:menu-key-spans
   #:hint-line-p #:wrap-hint-line #:fit-menu-lines
   ;; menu scrolling (long lists window with u/d + the scrollbar)
   #:+menu-page-size+ #:menu-window #:menu-window-pick
   #:menu-scroll #:menu-scrolled-lines #:*menu-scroll*
   ;; cell specials
   #:trigger-special #:run-special #:teleport-party
   #:trap-disarmed-flag
   ;; races (ability-score modifiers + which classes a race may take)
   #:define-race #:find-race #:races #:race-name #:race-title
   #:race-classes #:race-description #:race-image #:race-allows-class-p
   #:race-str #:race-dex #:race-iq #:race-con #:race-lck
   #:clamp-stat #:+stat-min+ #:+stat-max+
   ;; heroes and the party
   #:define-hero-class #:hero-classes
   #:make-hero #:hero-name #:hero-class #:hero-race #:hero-race-title
   #:hero-level
   #:hero-xp #:hero-max-hp #:hero-hp #:hero-max-sp #:hero-sp
   #:hero-caster-p #:hero-str #:hero-dex #:hero-iq
   #:hero-con #:hero-lck #:hero-ac #:hero-damage #:hero-gold
   #:hero-alive-p #:alive-heroes #:party-alive-p #:front-ranks
   #:hero-in-reach-p
   #:hero-extra-attacks #:hero-trap-skill
   #:hero-class-title #:hero-class-abbrev #:hero-summary-lines
   #:hero-image #:hero-image-path #:hero-sheet-lines #:hero-sheet-scroll
   ;; the sheet carousel's spells/songs page (a digit opens the card,
   ;; the card casts or plays)
   #:hero-magic-p #:make-magic-view #:magic-view-hero
   #:magic-view-pending #:magic-view-top
   #:magic-entries #:magic-lines #:magic-act
   #:+party-limit+ #:party-full-p #:join-party #:move-hero
   #:damage-hero #:heal-hero #:stat-bonus #:stat-gift #:saving-throw
   #:award-xp #:xp-for-level
   #:hero-level-up-pending-p #:advance-level
   #:pool-gold #:trade-gold
   ;; trading gold ('t' on the character sheet)
   #:make-trade-view #:trade-view-hero #:trade-view-to
   #:trade-view-amount #:trade-lines #:trade-act
   ;; items, inventory and equipment
   #:define-item #:find-item-type #:item-title #:item-usable-p
   #:item-fit-marker #:item-target-kind #:*item-kinds*
   #:+inventory-limit+ #:hero-items #:hero-equipped #:hero-carrying-p
   #:give-item #:drop-item #:pass-item #:discard-item
   #:equip-item #:unequip-item #:equipped-of-kind
   #:toggle-equip #:hero-attack-dice #:hero-missile-dice
   #:hero-effective-ac
   ;; the pack page ('i' on the character sheet — equip, 'p' to hand
   ;; an item to another party member, 't' to throw one away)
   #:make-equip-view #:equip-view-hero #:equip-view-top
   #:equip-view-mode #:equip-view-pending
   #:equip-lines #:equip-act
   ;; using items (torch, potion — the use menu)
   #:usable-items #:use-item
   #:make-use-view #:use-view-hero #:use-view-item #:use-view-top
   #:use-lines #:use-act
   ;; locations and shops
   #:game-location #:enter-location #:leave-location
   #:location-title #:location-kind #:location-arg
   #:location-image #:location-image-path
   #:cell-location-op #:facing-location-image-path
   #:shop-stock #:item-price #:item-sell-price #:buy-item #:sell-item
   #:make-shop-view #:shop-view-hero #:shop-view-mode #:shop-view-top
   #:shop-lines #:shop-act
   #:make-location-view #:location-lines #:location-act
   ;; spells
   #:define-spell #:find-spell-type #:spell-title
   #:spell-code #:spell-range #:spell-duration-text #:spell-target-kind
   #:spell-description #:spell-card-lines #:begin-cast
   #:spell-known-p #:spell-castable-p #:spells-for-hero #:cast-spell
   #:make-cast-view #:cast-view-hero #:cast-view-spell #:cast-view-top
   #:cast-view-dir #:cast-view-distance
   #:cast-lines #:cast-act #:*sp-regen-minutes*
   ;; bard songs
   #:define-song #:find-song-type #:song-title #:song-known-p
   #:song-description #:song-card-lines
   #:songs-for-hero #:sing-song #:current-song
   #:hero-singer-p #:hero-tunes #:hero-max-tunes
   #:make-sing-view #:sing-view-hero #:sing-view-top
   #:sing-lines #:sing-act
   ;; taverns (drinks refill a singer's tunes)
   #:tavern-price #:buy-drink #:tavern-lines #:tavern-act
   ;; temples (healing and raising, for gold) and the energy fount
   ;; (spell points at so many gold apiece)
   #:temple-price #:temple-raise-fee #:temple-cost #:temple-heal
   #:make-temple-view #:temple-view-patient #:temple-view-note
   #:temple-lines #:temple-act
   #:energy-price #:energy-cost #:energy-restore
   #:energy-lines #:energy-act
   ;; combat
   #:define-monster #:find-monster-type #:monster-type-name
   #:monster-type-level #:monster-type-ac #:monster-type-xp
   #:monster-type-item #:monster-type-item-chance
   #:monster-type-image
   #:monster-kind #:monster-hp #:monster-alive-p
   #:start-combat #:combat-round #:attempt-flee #:hero-can-attack-p
   #:*encounter-rate* #:maybe-wandering-encounter
   #:*idle-encounter-minutes* #:maybe-idle-encounter
   #:game-idle-encounter-clock
   #:combat-monsters #:alive-monsters #:combat-groups #:combat-banner
   #:combat-enemy-image #:combat-image-path
   #:*victory-image* #:*victory-linger* #:victory-image-path
   #:combat-round-no
   ;; combat round orders (the party engages or flees, every hero
   ;; picks, then the round runs)
   #:make-combat-orders #:combat-orders-hero #:combat-orders-chosen
   #:combat-orders-engaged
   #:combat-orders-review #:combat-orders-lines #:combat-orders-act
   ;; combat transcript speed (the +/- keys)
   #:*combat-speed* #:+combat-speed-max+
   #:combat-message-delay #:adjust-combat-speed
   ;; save games
   #:save-game #:load-game
   ;; named saves (the save/load slot menu)
   #:*save-dir* #:slot-path #:ensure-save-dir #:save-slots
   #:make-save-menu #:save-menu-mode #:save-menu-entry
   #:save-menu-lines #:save-menu-act
   ;; rendering
   #:render-dungeon #:render-game #:render-first-person #:beside
   #:map-edge-runs #:map-legend-entries #:*legend-markers*
   ;; the help screen
   #:help-lines
   ;; keyboard input normalization (see src/keys.lisp)
   #:vanilla-key-char
   ;; the microfont (the 7x7 display face, plus the condensed bold
   ;; 5x7 small face the engine's pages actually set)
   #:+microfont-advance+ #:+microfont-line-height+
   #:microfont-glyph #:microfont-text-width #:microfont-line
   #:+microfont-small-advance+ #:microfont-small-glyph
   #:microfont-small-text-width #:microfont-small-line
   ;; debug log (timestamped trace file, see src/debug-log.lisp)
   #:*debug-log-path* #:debug-log-enable #:debug-log-disable
   #:debug-log-enabled-p #:dlog #:dlog-timed
   ;; version (see src/version.lisp): the engine's own, and the slots
   ;; a game built on it fills in with its own, independent version
   #:+engine-version-major+ #:+engine-version-minor+ #:+engine-version-patch+
   #:*engine-name* #:*engine-version-date*
   #:engine-version #:engine-version-string
   #:*game-name* #:*game-version* #:*game-version-date*
   ;; interactive walkabout
   #:play #:play-amiga #:*gfx-dir*
   ;; engine home (self-located; default packs live under it)
   #:*engine-dir* #:engine-path))

(in-package :tale)
