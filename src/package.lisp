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
   #:map-page-window #:map-page-scroll
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
   ;; seams for tooling: the running game, and the two hooks a debug
   ;; build hangs itself off (see src/game.lisp)
   #:*game* #:*key-hook* #:*tick-hook*
   ;; game state / movement
   #:new-game #:game-map #:game-knowledge #:game-x #:game-y #:game-facing
   #:game-party #:game-flags #:game-combat
   #:game-effects #:add-effect #:remove-effect #:find-effect
   #:effect-name #:effect-expires-at #:effect-payload #:effect-label
   #:effect-image #:effect-image-path #:apply-effect-spec
   #:*timed-effect-keys* #:*instant-effect-keys*
   #:check-effect-spec #:effect-spec-combat-only-p
   #:effect-spec-reaches-foes-p #:*foe-facing-timed-keys*
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
   #:ambient-view-depth #:light-minutes-left #:light-depth
   #:light-brightness #:*light-fade-minutes*
   #:time-of-day #:game-time-of-day #:time-of-day-name #:time-of-day-line
   #:sky-color-for #:ground-color-for #:zone-pen-colors #:dim-rgb
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
   #:menu-line-text #:menu-line-key #:menu-line-spans
   #:menu-texts #:wrap-menu-line #:menu-key-spans
   #:hint-line-p #:wrap-hint-line #:fit-menu-lines
   ;; menu scrolling (long lists window with u/d + the scrollbar)
   #:+menu-page-size+ #:+book-page-size+ #:menu-window #:menu-window-pick
   #:menu-scroll #:menu-scrolled-lines #:*menu-scroll*
   ;; cell specials
   #:trigger-special #:run-special #:teleport-party
   #:trap-disarmed-flag
   ;; the question a cell may put (the ASK op): a page both front-ends
   ;; draw over the play page until it is answered
   #:game-question #:question-text #:question-ops
   #:ask-question #:answer-question #:question-lines #:question-act
   ;; named destinations (a homing spell's places)
   #:define-destination #:find-destination #:destinations
   #:travel-to-destination
   #:destination-name #:destination-title #:destination-map
   #:destination-x #:destination-y #:destination-facing
   ;; races (ability-score modifiers + which classes a race may take)
   #:define-race #:find-race #:races #:race-name #:race-title
   #:race-classes #:race-description #:race-image #:race-allows-class-p
   #:race-str #:race-dex #:race-iq #:race-con #:race-lck
   #:clamp-stat #:+stat-min+ #:+stat-max+
   ;; heroes and the party
   #:define-hero-class #:hero-classes #:startable-hero-classes
   #:make-hero #:hero-name #:hero-class #:hero-race #:hero-race-title
   #:hero-level
   #:hero-xp #:hero-max-hp #:hero-hp #:hero-max-sp #:hero-sp
   #:hero-caster-p #:hero-str #:hero-dex #:hero-iq
   #:hero-con #:hero-lck #:hero-ac #:hero-damage #:hero-gold
   #:hero-alive-p #:alive-heroes #:party-alive-p #:party-leader
   #:front-ranks
   #:hero-in-reach-p
   ;; ailments — the conditions a hero carries until something cures them
   #:*ailments* #:*poison-bite* #:ailment-p #:ailment-title #:ailment-noun
   #:hero-ailments
   #:hero-ailment-p #:hero-condition-titles #:hero-condition-code
   #:hero-helpless-p #:hero-can-act-p #:acting-heroes #:party-can-act-p
   #:afflict-hero #:cure-ailment #:cure-hero #:poison-bite
   #:hero-extra-attacks #:hero-trap-skill
   #:hero-class-title #:hero-class-abbrev #:hero-summary-lines
   #:hero-image #:hero-image-path #:hero-portrait
   #:hero-sheet-lines #:hero-sheet-scroll
   ;; the sheet carousel's spells/songs page (a digit opens the card,
   ;; the card casts or plays)
   #:hero-magic-p #:make-magic-view #:magic-view-hero
   #:magic-view-pending #:magic-view-refusal #:magic-view-top
   #:magic-entries #:magic-lines #:magic-act
   #:+party-limit+ #:party-full-p #:join-party #:move-hero
   #:damage-hero #:heal-hero #:stat-bonus #:stat-gift #:saving-throw
   #:award-xp #:xp-for-level
   ;; the experience ladder is the campaign's — the engine keeps the
   ;; mechanism and a gentle curve for a game that registers none
   #:define-xp-table #:*xp-table* #:*xp-growth*
   #:hero-level-up-pending-p #:advance-level
   ;; changing class — the second ladder; an art left behind freezes at
   ;; the level it was left and keeps granting what it opened
   #:hero-class-levels #:hero-class-level #:hero-held-class-p
   #:class-change-refusal #:hero-class-change-targets
   #:hero-can-change-class-p #:change-class
   #:pool-gold #:trade-gold
   ;; trading gold ('t' on the character sheet)
   #:make-trade-view #:trade-view-hero #:trade-view-to
   #:trade-view-amount #:trade-lines #:trade-act
   ;; items, inventory and equipment
   #:define-item #:find-item-type #:item-title #:item-usable-p
   #:item-fit-marker #:item-target-kind #:*item-kinds*
   #:+inventory-limit+ #:hero-items #:hero-equipped #:hero-carrying-p
   #:party-carrying-p #:party-carrier
   ;; the plot pieces (:QUEST) — carried outside the eight-slot limit
   #:item-type-quest #:quest-item-p #:pack-burden #:pack-gear
   #:hero-quest-items #:party-quest-items
   #:give-item #:drop-item #:pass-item #:discard-item
   #:equip-item #:unequip-item #:equipped-of-kind
   #:toggle-equip #:equipped-instance-p
   #:hero-attack-dice #:hero-missile-dice #:hero-missile-reach
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
   #:location-music #:location-music-path
   #:cell-location-op #:facing-location-image-path
   #:shop-stock #:item-price #:item-sell-price #:buy-item #:sell-item
   #:make-shop-view #:shop-view-hero #:shop-view-mode #:shop-view-top
   #:shop-lines #:shop-act
   #:make-location-view #:location-lines #:location-act
   #:notice-lines #:*notice-linger*
   ;; spells
   #:define-spell #:find-spell-type #:spell-title
   #:spell-code #:spell-range #:spell-reach #:spell-duration-text
   #:spell-target-kind
   #:spell-description #:spell-card-lines #:begin-cast #:spell-refusal
   #:spell-known-p #:spell-castable-p #:spells-for-hero #:cast-spell
   #:make-cast-view #:cast-view-hero #:cast-view-spell #:cast-view-top
   #:cast-view-dir #:cast-view-distance
   #:cast-lines #:cast-act #:*sp-regen-minutes*
   ;; bard songs
   #:define-song #:find-song-type #:song-title #:song-known-p
   #:song-description #:song-card-lines
   #:songs-for-hero #:sing-song #:current-song
   #:song-refusal #:song-playable-p
   #:hero-singer-p #:hero-tunes #:hero-max-tunes #:hero-tunes-text
   #:hero-sings-with #:hero-song-tool #:hero-tireless-p
   #:item-type-tireless
   #:make-sing-view #:sing-view-hero #:sing-view-top
   #:sing-lines #:sing-act
   ;; taverns (drinks refill a singer's tunes)
   #:tavern-price #:buy-drink #:tavern-lines #:tavern-act
   ;; temples (healing and raising, for gold) and the energy fount
   ;; (spell points at so many gold apiece)
   #:temple-price #:temple-raise-base #:temple-raise-rate
   #:temple-raise-fee #:temple-cures #:temple-cure-price #:temple-curable
   #:temple-wounds #:temple-cost #:temple-heal
   #:make-temple-view #:temple-view-patient #:temple-view-note
   #:temple-lines #:temple-act
   #:energy-price #:energy-cost #:energy-work-p #:energy-restore
   #:make-energy-view #:energy-view-hero #:energy-view-note
   #:energy-lines #:energy-act
   ;; combat
   #:define-monster #:find-monster-type #:monster-type-name
   #:monster-type-level #:monster-type-ac #:monster-type-xp
   #:monster-type-item #:monster-type-item-chance
   #:monster-type-inflicts #:monster-type-image
   #:monster-type-missile #:monster-type-missile-reach
   #:monster-type-missile-verb #:monster-type-speed
   #:monster-kind #:monster-hp #:monster-alive-p
   #:start-combat #:combat-round #:attempt-flee #:hero-can-attack-p
   #:hero-strike-function
   #:*encounter-rate* #:maybe-wandering-encounter
   #:*idle-encounter-minutes* #:maybe-idle-encounter
   #:game-idle-encounter-clock
   ;; distance: where the groups stand and how they close
   #:+melee-distance+ #:*combat-group-spacing* #:*combat-close-step*
   #:monster-group #:monster-distance #:monster-in-melee-p
   #:monster-can-shoot-p #:monster-step
   #:combat-distance #:nearest-monster #:monsters-in-reach
   #:group-monsters #:combat-group-indices #:group-label
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
   ;; the help screen, the quit confirmation and the magic-at-work page
   #:help-lines
   #:quit-confirm-lines #:quit-confirm-act
   #:make-workings-view #:workings-view-top #:workings-lines #:workings-act
   #:effect-payload-lines #:effect-time-left-text
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
