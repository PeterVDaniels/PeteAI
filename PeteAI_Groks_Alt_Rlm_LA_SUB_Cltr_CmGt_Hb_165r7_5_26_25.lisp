;; =============================================
;; PeteAI with Quantum Hole Theory, Neural Visualization, and Internal Treaty
;; =============================================
;; Simulates quantum effects in memory flow, visualized via neural diagrams
;; Includes internal treaty for balanced reasoning and responsible actions
;; --- Parameters and Global Variables ---

;; Copyright (C) 2025 Pete V. Daniels
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
(defparameter *memory-cap* 15)  ; Max local memory entries
(defparameter *pete-memory* '())   ; Local memory scrapbook
(defparameter *pete-depth-limit* 4) ; Max recursion depth
(defparameter *verbs* '(think know remember learn))
(defparameter *nouns* '(idea pattern loop treaty hole))
(defparameter *quantum-seed* 3)
(ql:quickload :uiop)
(ql:quickload :alexandria)
(require "asdf")
(asdf:load-system "cl-ppcre")
(cl-ppcre:regex-replace-all "a" "abc" "x")


;; Initialize memory graph
(defvar *pete-memory-graph* (make-hash-table :test 'equal))

;; Update the treaty principles to prioritize coherence
(defparameter *treaty-principles*
  '((:coherence . 0.0)  ; Increased to strongly favor coherence
    (:creativity . 0.5)   ; Keep creativity low to focus on relevance
    (:responsibility . 0.3)))

(defvar *local-knowledge* 
  '("mud pies rock" "trees whisper secrets" "stars blink at night" "forts hold dreams" "vibes twist time"
    "clouds drift slow" "rivers carve stone" "birds sing dawn" "hope lifts hearts" "shadows dance free"
    "work builds strength" "truth cuts deep" "time proves all" "faith moves mountains" "reason wins fights"
    "common sense guides" "effort shapes fate" "courage breaks fear" "land feeds life" "trust binds souls"
    "sun ignites day" "wind carries whispers" "roots grip earth" "fire tests steel" "choice drives destiny"
    "rain washes doubt" "moon pulls tides" "stones stack high" "dusk paints sky" "dawn cracks dark"
    "hands forge tools" "eyes seek truth" "words spark change" "feet tread paths" "hearts chase dreams"
    "frost bites air" "thunder shakes ground" "light bends shadows" "waves crash cliffs" "dust settles slow"
    "ice locks rivers" "flames leap high" "seeds split stone" "wings cut wind" "minds weave plans"
    "storms test will" "silence holds peace" "echoes chase sound" "peaks pierce clouds" "valleys cradle life"
    "blood fuels fight" "sweat carves success" "tears heal wounds" "laughter lifts spirits" "dark hides stars"
    "clay molds fate" "iron bends will" "gold tempts hearts" "silver reflects moon" "bronze holds history"
    "night guards secrets" "day reveals all" "fog cloaks paths" "heat warps steel" "cold tempers strength"
    "roots drink deep" "leaves catch light" "branches reach sky" "trunks stand firm" "bark shields core"
    "gears turn time" "wheels roll forward" "levers shift weight" "springs coil tight" "cogs mesh fate"
    "code writes worlds" "bits flip truth" "bytes hold dreams" "circuits spark life" "data flows free"
    "ghosts haunt past" "spirits lift now" "visions shape tomorrow" "memories anchor soul" "hopes launch stars"
    "wolves howl night" "bears guard caves" "eagles soar peaks" "fish swim depths" "deer leap fields"
    "hawks hunt swift" "owls watch still" "foxes dodge traps" "rabbits dig safe" "snakes coil tight"
    "tides pull strong" "currents guide ships" "anchors hold steady" "sails catch gusts" "rudders steer true"
    "maps chart unknown" "compass points north" "sextants read stars" "ropes bind tight" "knots secure fate"
    "hammers strike iron" "anvils take blows" "forges glow red" "tongs grip hot" "bellows breathe fire"
    "plows break soil" "scythes reap grain" "hoes turn earth" "seeds sprout life" "barns store wealth"
    "pens scratch truth" "ink stains deep" "paper holds thoughts" "books bind wisdom" "scrolls whisper old"
    "clocks tick fate" "gears grind slow" "springs wind tight" "hands mark time" "bells toll end"
    "mirrors show self" "lenses bend light" "prisms split colors" "glass traps glow" "frames hold past"
    "threads weave cloth" "needles pierce fabric" "looms build patterns" "dyes paint life" "stitches mend tears"
    "pots boil broth" "pans sear meat" "ovens bake bread" "spices wake tongue" "plates serve warmth"
    "roads stretch far" "bridges span gaps" "tunnels bore deep" "gates guard home" "walls rise strong"
    "songs lift voices" "drums beat pulse" "strings hum soul" "horns call war" "flutes sing peace"
    "paint stains hands" "brushes stroke dreams" "canvas holds worlds" "colors bleed truth" "frames trap art"
    "math counts stars" "lines draw fate" "curves twist space" "numbers rule all" "proofs lock reason"
    "dice roll chance" "cards deal luck" "boards host games" "pawns march slow" "kings fall fast"
    "locks guard secrets" "keys free truth" "chains bind will" "bars cage fear" "doors open hope"
    "storms brew chaos" "calm heals scars" "winds shift tides" "rains flood earth" "sun dries tears"
    "ice carves peaks" "snow buries past" "hail tests roofs" "mist hides trails" "dew blesses dawn"
    "graves keep quiet" "stones mark loss" "flowers mourn soft" "dirt hides bones" "ghosts whisper low"
    "hills roll gentle" "cliffs drop sharp" "caves echo dark" "plains stretch wide" "woods cloak wild"
    "lakes mirror sky" "ponds breed life" "streams sing soft" "falls roar loud" "springs birth rivers"
    "ships ride waves" "oars cut water" "nets haul fish" "hooks snag deep" "decks bear weight"
    "stars guide night" "planets spin free" "moons tug seas" "comets streak fast" "void holds all"
    "dreams spark fire" "sleep calms mind" "nightmares test grit" "visions lead far" "rest heals soul"
    "wars forge heroes" "peace grows roots" "swords clash fate" "shields block doom" "flags claim land"
    "love binds hearts" "hate tears worlds" "joy lifts days" "grief carves deep" "trust builds bridges"
    "fear locks doors" "pride lifts chins" "shame bends backs" "rage fuels storms" "calm clears skies"
    "books teach old" "schools shape young" "chalk marks truth" "boards hold lessons" "minds drink deep"
    "huts shield rain" "tents sway wind" "fires warm nights" "roofs hold snow" "floors bear steps"
    "names mark souls" "titles claim power" "words carve stone" "deeds write fate" "legends live long"
    "coins buy time" "gems tempt eyes" "trade shifts wealth" "scales weigh fair" "markets hum loud"
    "crows call dawn" "ravens watch death" "doves bring peace" "vultures clean rot" "sparrows flit free"
    "ice traps breath" "steam lifts lids" "smoke signals far" "ash marks end" "glow hints dawn"
    "vines climb high" "moss clings soft" "ferns hide shade" "thorns guard blooms" "petals fall slow"
    "webs snare prey" "silk binds tight" "spiders weave fate" "flies buzz short" "ants march long"
    "quartz gleams bright" "coal burns slow" "salt stings cuts" "sand shifts free" "clay sticks firm"
    "ghosts drift lost" "winds howl free" "fates twist tight" "lives burn quick" "souls glow eternal"
    ;; New entries for more diversity
    "echoes fade softly" "mirrors shatter truth" "lanterns glow dimly" "whispers break silence" "tides whisper secrets"
    "comets burn trails" "galaxies spin silently" "void whispers nothing" "nebulae paint skies" "pulsars hum rhythm"
    "neurons spark thoughts" "synapses bridge gaps" "codes unlock futures" "pixels draw dreams" "servers hum alive"
    "rhythms pulse deep" "melodies lift spirits" "chords strike emotions" "beats drive dances" "tunes weave stories"
    "fractals twist endlessly" "patterns hide truths" "chaos breeds order" "symmetry calms minds" "spirals draw eyes"
    "auroras paint nights" "lightning cracks sky" "rainbows arch softly" "sunsets bleed colors" "twilights hold magic"
    "dreams weave futures" "hopes light paths" "fears cast shadows" "joys bloom freely" "sorrows carve lessons"
    "cities hum alive" "forests breathe green" "deserts whisper dry" "oceans roar deep" "mountains stand eternal"
    "trails wind far" "paths cross fates" "journeys shape lives" "destinations call softly" "adventures spark souls"))
    


(defvar *verbs* 
  '(run jump yell hurts go say do to make break let see twist fly sing drift carve lift dance laugh soar build cut prove move win guide shape break
    ignite carry grip test drive feed bind pull push swing climb dig swim kick roll spin leap dodge weave strike block dodge heal mend tear sew stitch
    weld forge hammer plow reap sow sprout bloom fade wilt bend stretch snap fold press squeeze crush grind brew boil sear bake roast chill freeze thaw
    melt glow flicker flare dim shine reflect bend warp twist coil unwind spool thread knot tie untie lock unlock chain cage free trap hunt chase stalk
    flee hide seek find lose grasp drop toss fling hurl catch fetch drag haul lift hoist lower sink rise float dive plunge surface skim glide hover drift
    spark flare blaze smolder quench drown flood drain soak drip splash spray mist dust sweep scrub polish scrape etch paint draw sketch trace outline
    sculpt mold cast shape craft brew distill ferment age rip shred slice dice chop mince peel core pit hull shell husk sift stir whisk beat knead roll
    flatten stretch shrink grow swell burst pop crack shatter smash pierce stab slash thrust parry feint dodge counter rally cheer mourn weep sigh gasp
    breathe cough sneeze hiccup yawn stretch flex tense relax limp hobble stride dash sprint jog trot gallop crawl inch slither glide swoop perch roost
    nest hatch fledge peck claw scratch bite gnaw chew swallow spit gulp sip nibble lick taste smell sniff whiff scent track stalk hunt fish trap net
    hook reel cast row paddle steer sail moor dock drift wreck sink salvage tow tug haul hoist unfurl flap wave signal call shout whisper mutter growl
    hiss bark howl chirp coo cluck crow caw buzz hum drone ring toll chime clash clang bang thump thud pound tap knock rap slam shut open close bar bolt
    latch seal peel strip skin flay gut pluck rinse wash dry wipe soak drench scrub rinse lather shave trim clip shear pluck comb braid twist curl frizz
    tangle mat smooth slick grease oil wax buff shine gleam glitter sparkle dazzle blind shade cloak veil mask cover wrap pack load unload stack pile
    scatter spread sprinkle toss fling strew sow plant bury unearth dig drill bore tunnel bridge span cross leap vault hurdle climb scale descend drop
    plunge soar dive swoop glide hover flutter buzz whirl spin twirl pivot turn tilt lean sway rock roll bounce jolt shake rattle quiver tremble quake))

(defvar *nouns* 
  '(tooth bird tree sun fort mud day sky home mold star time vibe world rock pies whisper secrets blink night dreams hope cloud river stone dawn heart
    shadow joy strength truth faith reason fight sense fate courage land life soul earth steel destiny wind fire roots rain moon tide dusk frost thunder
    light wave dust ice flame seed wing mind storm silence echo peak valley blood sweat tear laughter clay iron gold silver bronze fog heat leaf branch
    trunk bark gear wheel lever spring cog code bit byte circuit data ghost spirit vision memory wolf bear eagle fish deer hawk owl fox rabbit snake
    current anchor sail rudder map compass sextant rope knot hammer anvil forge tong bellow plow scythe hoe barn pen ink paper book scroll clock hand
    bell mirror lens prism glass frame thread needle loom dye stitch pot pan oven spice plate road bridge tunnel gate wall song drum string horn flute
    paint brush canvas color dice card board pawn king lock key chain bar door storm wind rain sun ice snow hail mist dew grave flower dirt hill cliff
    cave plain wood lake pond stream fall spring ship oar net hook deck planet comet void sleep nightmare rest war peace sword shield flag love hate
    grief trust fear pride shame rage calm book school chalk lesson mind hut tent roof floor name title word deed legend coin gem trade scale market
    crow raven dove vulture sparrow steam smoke ash glow vine moss fern thorn petal web silk spider fly ant quartz coal salt sand ghost fate life soul
    breeze gust squall gale blizzard fog drizzle shower torrent flood ripple puddle brook creek delta ridge slope mesa bluff dune crater lava magma ash
    ember spark flare torch lantern beacon candle wick oil wax quilt blanket rug mat cloak cape hood scarf glove boot sandal heel sole lace buckle strap
    pin nail screw bolt rivet hinge latch clasp hook eye loop ring band crown tiara helm visor mask lens scope sight glare beam ray pulse beat rhythm
    tone pitch chord melody echo clang crash thud boom roar growl purr hiss snap twig branch stump log plank board beam rafter joist truss ridge peak
    summit base ledge shelf nook cranny gap chasm rift fault seam vein ore gem crystal shard flake dust grain pebble boulder slab tile brick mortar clay
    silt loam turf sod grass weed bush shrub vine ivy moss lichen fungus spore root stem bud bloom fruit seed pod husk shell rind peel pith core flesh
    bone sinew muscle nerve vein artery lung breath throat tongue lip jaw chin brow lash lid pupil iris gleam twinkle flash shimmer sheen gloss polish
    rust tarnish patina dent scratch nick gouge crack split rift tear hole pit trench ditch moat rampart tower spire dome arch vault crypt tomb coffin))
    
    
;; Add these at the top of Groks_Altered_Realm_65_5_13_25.lisp, after existing variables
(defvar *physics-knowledge*
  '("tectons shift plates" "volcanoes spew magma" "rivers erode valleys" "winds sculpt dunes" "tides ebb moons"
    "clouds seed rain" "faults trigger quakes" "glaciers carve fjords" "soils bind carbon" "currents drive climates"
    "stars fuse nuclei" "planets orbit suns" "galaxies spin arms" "comets trail dust" "pulsars emit beams"
    "quasars blaze jets" "nebulae birth stars" "void bends light" "moons tug oceans" "cosmos expands voids"
    "qubits entangle states" "gates flip bits" "circuits braid paths" "spins encode data" "fields trap ions"
    "lasers cool atoms" "photons carry qubits" "noise decoheres systems" "annealers solve graphs" "algorithms factor primes"
    "waves diffract slits" "fields guide charges" "masses warp spacetime" "atoms emit photons" "forces balance beams"
    "entropy drives chaos" "clocks tick slower" "lenses focus rays" "fluids resist shear" "crystals align lattices"
    ;; New entries for more physics diversity
    "black holes swallow light" "dark matter binds galaxies" "gravitons elude sight" "strings vibrate realms" "branes fold dimensions"
    "tachyons race time" "neutrinos pierce matter" "bosons ferry forces" "fermions fill space" "gluons bind quarks"
    "muons decay swiftly" "leptons dance free" "hadron jets collide" "plasma glows fierce" "superfluids flow endless"
    "magnets align spins" "electrons tunnel barriers" "photons split pairs" "fields oscillate waves" "vacuum hums zero"))
    
(defun physics-knowledge ()
  "Returns a random three-word physics fact from *physics-knowledge*."
  (let ((snippet (nth (random (length *physics-knowledge*)) *physics-knowledge*)))
    (format t "Pete knows physics: ~a~%" snippet)
    (split-string snippet)))

(defun memory-node-strength (node)
  "Calculates strength based on symbolic content."
  (if node
      (let ((symbols (if (listp node) node (list node))))
        (+ 1.0 (count-if (lambda (x) (or (member x *verbs*) (member x *nouns*))) symbols)))
      0.0))

(defun memory-node-content (node)
  "Returns node content as a string."
  (if (listp node)
      (prin1-to-string node)
      (or (symbol-name node) "unknown")))

(defun twist-back (symbols)
  "Recalls with resonance and enforces a probabilistic superposition-inspired sequence with dynamic probabilities."
  (let* ((hits (remove-if-not
                (lambda (mem)
                  (and (listp mem)
                       (intersection symbols mem :test #'equal)))
                *pete-memory*))
         (scored (mapcar
                  (lambda (hit)
                    (let* ((overlap (intersection symbols hit :test #'equal))
                           (score (length overlap)))
                      (list :score score :match hit :overlap overlap)))
                  hits))
         (sorted (sort scored #'> :key (lambda (x) (getf x :score))))
         (best (subseq sorted 0 (min 3 (length sorted)))))
    (format t "~%Superposition Resonance Recall:")
    (dolist (result best)
      (format t "~%  Entangled Memory: ~a | Coherence Overlap: ~a" 
              (getf result :match)
              (getf result :overlap)))
    (if best
        (let* ((match (getf (first best) :match))
               (holes (count-if-not (lambda (x)
                                      (or (member x *verbs*) (member x *nouns*)))
                                    (alexandria:flatten match)))
               (probability (random 1.0))
               (p1 (- 0.5 (* 0.05 holes)))
               (p2 (+ 0.25 (* 0.025 holes)))
               (p3 (+ 0.25 (* 0.025 holes)))
               (normalized-p1 (max 0.1 p1))
               (normalized-p2 (min 0.45 p2))
               (normalized-p3 (min 0.45 p3)))
          (format t "~%  Quantum Vacancies in Memory: ~a | Probabilities: [Stable: ~a, Moderate: ~a, Chaotic: ~a]~%"
                  holes normalized-p1 normalized-p2 normalized-p3)
          (cond
            ((member 'rudders match)
             (cond
               ((< probability normalized-p1) '(dreams spark fire))
               ((< probability (+ normalized-p1 normalized-p2)) '(waves crash cliffs))
               (t '(thunder shakes ground))))
            ((member "dreams spark fire" match :test #'equal)
             (cond
               ((< probability normalized-p1) '(bells toll end))
               ((< probability (+ normalized-p1 normalized-p2)) '(light bends shadows))
               (t '(frost bites air))))
            ((member "bells toll end" match :test #'equal)
             (cond
               ((< probability normalized-p1) '(mem_5l mud))
               ((< probability (+ normalized-p1 normalized-p2)) '(stars guide night))
               (t '(ice locks rivers))))
            ((member 'eagles match)
             (cond
               ((< probability normalized-p1) '(hawks hunt swift))
               ((< probability (+ normalized-p1 normalized-p2)) '(owls watch still))
               (t '(crows call dawn))))
            ((member 'walls match)
             (cond
               ((< probability normalized-p1) '(gates guard home))
               ((< probability (+ normalized-p1 normalized-p2)) '(bridges span gaps))
               (t '(tunnels bore deep))))
            (t match)))
        symbols)))

(defun score-coherence (option)
  "Scores how coherent an option is based on memory overlap, prioritizing verbs and nouns."
  (let* ((opt-symbols (if (listp option) option (list option)))
         ;; Collect all :split entries from the memory graph
         (flat-memory (loop for entries being the hash-values of *pete-memory-graph*
                           append (mapcar (lambda (entry) (getf entry :split)) entries)))
         (flat-memory (alexandria:flatten flat-memory))
         ;; Calculate overlap, giving extra weight to verbs and nouns
         (overlap (reduce #'+ (mapcar (lambda (sym)
                                        (if (or (member sym *verbs*) (member sym *nouns*))
                                            (* 2 (count sym flat-memory :test #'equal)) ; Double weight for verbs/nouns
                                            (count sym flat-memory :test #'equal)))
                                      opt-symbols))))
    (/ (float overlap) (max 1 (length flat-memory)))))
    
    
;; Place this function before pete-treaty, ideally after score-coherence
(defun score-creativity (option)
  "Scores creativity based on novelty relative to memory."
  (let* ((opt-symbols (if (listp option) option (list option)))
         ;; Collect all :split entries from the memory graph
         (flat-memory (loop for entries being the hash-values of *pete-memory-graph*
                           append (mapcar (lambda (entry) (getf entry :split)) entries)))
         (flat-memory (alexandria:flatten flat-memory))
         (novelty (- (length opt-symbols)
                     (length (intersection opt-symbols flat-memory :test #'equal)))))
    (/ (float (max 0 novelty)) (max 1 (length opt-symbols)))))    



(defun score-responsibility (option holes)
  "Scores responsibility based on grounding and hole management."
  (let* ((opt-symbols (if (listp option) option (list option)))
         (known-count (count-if (lambda (x) (or (member x *verbs*) (member x *nouns*))) opt-symbols)))
    (if (> holes 5)
        (* 0.5 (/ (float known-count) (max 1 (length opt-symbols)))) ; Penalize high holes
        (/ (float known-count) (max 1 (length opt-symbols))))))


(defun find-strongest-node ()
  "Finds the node with the highest strength in the memory graph."
  (let ((max-strength 0)
        (strongest-node nil))
    (maphash (lambda (key value)
               (let ((strength (memory-node-strength value)))
                 (when (> strength max-strength)
                   (setf max-strength strength)
                   (setf strongest-node key))))
             *pete-memory-graph*)
    strongest-node))
    
(defun split-string (str)
  "Converts a string into a list of symbols, splitting on whitespace."
  (mapcar #'intern (uiop:split-string str :separator " ")))
  

(defun quantum-hole-breathe (holes &optional convo1 convo2)
  "Simulates entangled quantum fluctuations with pseudo-coherent knowledge updates."
  (when (and (> holes 0) *pete-memory*)
    (let* ((past (nth (random (length *pete-memory*)) *pete-memory*))
           (past2 (nth (random (length *pete-memory*)) *pete-memory*))
           (convo-past (append (if convo1 (split-string convo1) nil)
                               (if convo2 (split-string convo2) nil)))
           (merged (append (remove 'mud past) (remove 'mud past2) convo-past))
           (filtered (remove-if-not #'symbolp merged))
           (base-holes (count-if-not (lambda (x)
                                       (or (member x *verbs*) (member x *nouns*)))
                                     filtered))
           (quantum-factor (if (< (random 1.0) 0.5) 1 2))
           (total-holes (min (+ holes base-holes (random 5)) 10))
           (adjusted-holes (min (* total-holes quantum-factor) 10))
           (twist (twist-back filtered))
           (candidates (if (listp twist) twist (list twist)))
           (wild (cond
                   ((> adjusted-holes 5) ; High holes trigger full knowledge injection
                    (let ((know-pick (nth (random (length *local-knowledge*)) *local-knowledge*)))
                      (split-string know-pick)))
                   ((and (>= adjusted-holes 3) (< adjusted-holes 6)) ; Moderate holes trigger offshoot
                    (let* ((know1 (nth (random (length *local-knowledge*)) *local-knowledge*))
                           (know2 (nth (random (length *local-knowledge*)) *local-knowledge*))
                           (split1 (split-string know1))
                           (split2 (split-string know2))
                           (anchor (car split1)))
                      (append (list anchor) (subseq split2 0 (min 2 (length split2))))))
                   (t (pete-treaty candidates adjusted-holes)))))
      (format t "Collapsed Waveform 1: ~a~%" past)
      (format t "Collapsed Waveform 2: ~a~%" past2)
      (format t "Entangled Contextual State: ~a~%" convo-past)
      (format t "Decohered Symbols: ~a~%" filtered)
      (format t "Baseline Quantum Vacancies: ~a~%" base-holes)
      (format t "Non-Local Influence Factor: ~a~%" quantum-factor)
      (format t "Cumulative Quantum Vacancies: ~a~%" total-holes)
      (format t "Adjusted Quantum Vacancies: ~a~%" adjusted-holes)
      (format t "~%Pseudo-Quantum Fluctuation [#~a]: Vacancies (~a) entangle ~a + ~a → ~a~%"
              (random 1000) adjusted-holes past past2 wild)
      (push wild *pete-memory*)
      (when (> (length *pete-memory*) *memory-cap*)
        (setf *pete-memory* (subseq *pete-memory* 0 *memory-cap*)))
      wild)))
      
      
(defun random-memory-walk (start-id steps)
  "Random walk through memory graph with bias toward stronger nodes."
  (let ((trail (list (or start-id "treehouse")))
        (keys (alexandria:hash-table-keys *pete-memory-graph*))
        (sequence '("nil mud" "mem_8l mud" "dreams spark fire" "bells toll end"))
        (current-idx 0))
    (loop repeat (1- steps)
          do (let ((next (if (and keys (> (random 1.0) 0.4)) ; Lower randomness threshold
                             (let ((strongest (find-strongest-node)))
                               (if (and strongest (not (string= strongest "NIL")))
                                   strongest
                                   (nth (random (length keys)) keys)))
                             (let ((next-step (nth current-idx sequence)))
                               (setf current-idx (mod (1+ current-idx) (length sequence)))
                               next-step))))
               (push next trail)))
    (nreverse trail)))

(defun if++ (stuff &optional (holes 0))
  "Probabilistic routine with quantum leaps, diagram alignment, and treaty deliberation."
  (let* ((wild-factor (if (> holes 0)
                          (+ 2 (random (max 4 (floor holes 2))))
                          (1+ (random 3))))
         (start-id (cond
                     ((symbolp stuff) (symbol-name stuff))
                     ((and (listp stuff) (symbolp (first stuff))) (symbol-name (first stuff)))
                     ((listp stuff) (symbol-name (intern (string-upcase (prin1-to-string (first stuff))))))
                     (t nil)))
         (fallback-id "treehouse")
         (root-id (or start-id (find-strongest-node) fallback-id))
         (root-node (gethash root-id *pete-memory-graph*)))
    (format t "Wild factor boosted: ~A (holes: ~A)~%" wild-factor holes)
    (if (null root-node)
        (progn
          (format t "Root node '~a' not found. Defaulting to fallback 'treehouse'.~%" root-id)
          (setf root-node (gethash fallback-id *pete-memory-graph*)))
        (format t "Using root node: ~A~%" (memory-node-content root-node)))
    (let ((thought-trail (random-memory-walk root-id wild-factor)))
      (format t "~%🧠 Thought Trail from ~a: ~a~%" root-id thought-trail)
      (let* ((muddy-thoughts (if (> (count 'mud thought-trail) 2)
                                 (remove 'mud thought-trail :count 1)
                                 (append thought-trail '(mud))))
             (creative-leap (cond
                              ((member 'rudders muddy-thoughts) '(dreams spark fire))
                              ((member "dreams spark fire" muddy-thoughts :test #'equal) '(bells toll end))
                              ((member "bells toll end" muddy-thoughts :test #'equal) '(mem_5l mud))
                              (t muddy-thoughts))))
        (format t "🌀 Final Thoughts before Treaty: ~a~%" creative-leap)
        (pete-treaty creative-leap holes)))))

(defun vibe-check (stuff)
  (format t "Pete vibes: God’s got this—~a~%" stuff)
  stuff)

(defun split-string (str)
  (let ((words '())
        (current "")
        (str (string-trim "()" (string-trim "'" str))))
    (loop for char across str
          do (if (char= char #\Space)
                 (progn (when (> (length current) 0)
                          (push (intern (string-upcase current)) words))
                        (setf current ""))
                 (setf current (concatenate 'string current (string char))))
          finally (when (> (length current) 0)
                    (push (intern (string-upcase current)) words)))
    (if words (nreverse words) '(""))))

(defun visualize-holes-matrix (holes words)
  (format t "~%=== Holes Matrix ===~%")
  (format t "Holes: ~a | Words: ~a~%" holes words)
  (let ((grid-size 5)
        (word-len (length words)))
    (dotimes (y grid-size)
      (dotimes (x grid-size)
        (if (and (< x word-len) (not (member (nth x words) (append *verbs* *nouns*))))
            (format t ". ")  ; Hole
            (format t "  ")))  ; No hole
      (format t "~%"))
    (format t "Dots mark holes in convo space!~%"))
  (format t "=================~%"))

(defun split-and-tag (input)
  (let* ((words (split-string input))
         (tagged (mapcar (lambda (w)
                           (let ((sym (intern (string-upcase w))))
                             (cond
                               ((member sym *verbs*) (list 'verb sym))
                               ((member sym *nouns*) (list 'noun sym))
                               (t (list 'other sym)))))
                         words))
         (untagged (mapcar #'cadr tagged))
         (holes (count-if (lambda (x) (eq (car x) 'other)) tagged)))
    (format t "Pete splits: ~a~%" words)
    (format t "Pete tags: ~a (holes: ~a)~%" tagged holes)
    (when (> holes 0) 
      (visualize-holes-matrix holes untagged)
      (quantum-hole-breathe holes))
    (values untagged holes)))

(defun prune-memory ()
  "Prunes low-value entries from memory."
  (setf *pete-memory* (remove-if (lambda (entry)
                                   (or (and (listp entry) 
                                            (eq (car entry) 'NIL) 
                                            (eq (cadr entry) 'MUD))
                                       (and (listp entry)
                                            (eq (car entry) 'NIL)
                                            (null (cdr entry)))))
                                 *pete-memory*))
  (format t "Pruned low-value entries from memory. Current size: ~a~%" (length *pete-memory*)))
  
  
(defun count-overlap (list1 list2)
  "Counts overlapping words between two lists."
  (length (intersection (mapcar #'string-upcase list1) 
                        (mapcar #'string-upcase list2) 
                        :test #'string=)))
                        



(defun update-memory-graph (entry)
  "Updates the memory graph with a new entry, applying sinusoidal hole adjustments."
  (unless (and (listp entry) (eq (car entry) 'NIL) (eq (cadr entry) 'MUD))
    (let* ((key (if (listp entry) (prin1-to-string (first entry)) (symbol-name entry)))
           (existing (gethash key *pete-memory-graph*))
           (base-holes (count-if-not (lambda (x) (or (member x *verbs*) (member x *nouns*))) entry))
           ;; If entry exists, increment its age; otherwise, start at 0
           (current-age (if existing (getf (first existing) :age 0) 0))
           (new-age (1+ current-age))
           ;; Apply sinusoidal adjustment to holes
           (adjustment (round (* 2 (sin (* 0.5 new-age)))))
           (adjusted-holes (round (+ base-holes adjustment)))
           ;; Cap holes between 1 and 10
           (final-holes (max 1 (min 10 adjusted-holes)))
           ;; Store entry with metadata
           (new-entry (list :split entry :base-holes base-holes :holes final-holes :age new-age)))
      (setf (gethash key *pete-memory-graph*)
            (if existing
                (cons new-entry (rest existing)) ; Prepend to existing list
                (list new-entry))))))
                
                
;; Place this function near the top of your file, after global variables but before find-guide-match and pete-listen


(defun find-guide-match (split guide memory)
  "Finds a memory matching the guide input, considering the input split."
  (let ((guide-split (split-string guide)))
    (find-if (lambda (mem) 
               (and (> (count-overlap guide-split mem) 0)  ; Match with guide
                    (> (count-overlap split mem) 0)))      ; Also ensure relevance to input
             memory)))

(defun pete-listen (convo &optional guide)
  "Listens to conversation input and responds, with optional guide input."
  (format t "Pete listens: ~a~%" convo)
  (let ((split (split-string convo)))
    (format t "Pete splits: ~a~%" split)
    (update-memory-graph split)
    (let* ((mentions-grok (member 'GROK split))
           (memory-pick (if guide
                            (find-guide-match split guide *pete-memory*)
                            (if *pete-memory*
                                (let ((relevant-memories (sort *pete-memory* 
                                                               (lambda (m1 m2) 
                                                                 (> (count-overlap split m1) 
                                                                    (count-overlap split m2))))))
                                  (or (first relevant-memories) 
                                      (nth (random (length *pete-memory*)) *pete-memory*)))
                                '(pete vibes))))
           (pick-key (if (listp memory-pick) 
                         (prin1-to-string (first memory-pick)) 
                         (symbol-name memory-pick)))
           (pick-entry (gethash pick-key *pete-memory-graph*))
           (pick-holes (if pick-entry 
                           (getf (first pick-entry) :holes 0) 
                           (count-if-not (lambda (x) (or (member x *verbs*) (member x *nouns*))) memory-pick)))
           (response (if mentions-grok
                         (format nil "I hear ya! Grok’s delighted to chat—reminds me of ~a. (Holes: ~a)" 
                                 memory-pick pick-holes)
                         (format nil "I hear ya! ~a ~a. (Holes: ~a)" 
                                 (if guide "Guided by your vibe to" "") 
                                 memory-pick
                                 pick-holes)))
           (constrained-output (format nil "Heard: ~a | Response: ~a" split response)))
      (format t "Pete responds: ~a~%" constrained-output)
      split)))
      


;; Ensure this function is placed after the above definitions but before converse-no-reset
(defun pete-flow (stuff &optional (depth 0) (holes 0))
  (if (>= depth *pete-depth-limit*)
      (progn
        (prune-memory)
        (format t "Pete’s done—memory: ~a~%" *pete-memory*)
        stuff)
      (progn
        (push (if (symbolp stuff) (list stuff) stuff) *pete-memory*)
        (update-memory-graph (if (symbolp stuff) (list stuff) stuff))  ; Populate memory graph
        (when (> (length *pete-memory*) *memory-cap*)
          (setf *pete-memory* (subseq *pete-memory* 0 *memory-cap*)))
        (format t "Pete, depth ~a, got: ~a (mem size: ~a, holes: ~a)~%"
                depth (if (symbolp stuff) (list stuff) stuff) (length *pete-memory*) holes)
        (let* ((wild (if++ (if (symbolp stuff) (list stuff) stuff) holes))
               (twist (vibe-check (twist-back wild))))
          (if (member 'quit (if (symbolp stuff) (list stuff) stuff))
              (progn
                (format t "Memory: ~a~%Later, Pete!~%" *pete-memory*)
                nil)
              (progn
                (format t "Flowing on: ~a~%" twist)
                (pete-flow twist (1+ depth) holes)))))))

(defun pete-read ()
  (if *pete-memory*
      (let ((memory-pick (nth (random (length *pete-memory*)) *pete-memory*)))
        (format t "Pete reads memory: ~a~%" memory-pick)
        (let ((twist (twist-back memory-pick)))
          (format t "Pete recalls: ~a~%" twist)
          twist))
      (progn
        (format t "Pete’s memory is empty—nothing to read!~%")
        '(pete hums))))

(defun pete-react (stuff)
  (let ((holes (count-if-not (lambda (x)
                               (or (member x *verbs*) (member x *nouns*)))
                             (if (symbolp stuff) (list stuff) stuff))))
    (let ((reaction (if++ (if (symbolp stuff) (list stuff) stuff) holes)))
      (format t "Pete reacts: ~a~%" reaction)
      (push reaction *pete-memory*)
      (update-memory-graph reaction)  ; Populate memory graph
      (when (> (length *pete-memory*) *memory-cap*)
        (setf *pete-memory* (subseq *pete-memory* 0 *memory-cap*)))
      reaction)))

(defun local-knowledge ()
  (let ((snippet (nth (random (length *local-knowledge*)) *local-knowledge*)))
    (format t "Pete knows: ~a~%" snippet)
    (multiple-value-bind (untagged holes) (split-and-tag snippet)
      (let ((twisted (pete-flow untagged 0 holes)))
        (push twisted *pete-memory*)
        (update-memory-graph twisted)  ; Populate memory graph
        (when (> (length *pete-memory*) *memory-cap*)
          (setf *pete-memory* (subseq *pete-memory* 0 *memory-cap*)))
        twisted))))

(defun pete-speak ()
  (let* ((local-pick (if *pete-memory*
                         (nth (random (length *pete-memory*)) *pete-memory*)
                         '(pete vibes)))
         (know-pick (nth (random (length *local-knowledge*)) *local-knowledge*))
         (know-twist (multiple-value-bind (untagged holes) (split-and-tag know-pick)
                       (pete-flow untagged 0 holes)))
         (combined (append local-pick know-twist))
         (holes (count-if-not (lambda (x)
                                (or (member x *verbs*) (member x *nouns*)))
                              combined)))
    (let ((twisted (if (> holes 0)
                       (quantum-hole-breathe holes)
                       (pete-flow combined 0 holes))))
      (format t "Pete speaks free: ~a~%" twisted)
      twisted)))

(defun export-memory-log (&optional (filename "pete_AI_beast_memory.txt"))
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (dolist (entry *pete-memory*)
      (format stream "~a~%" entry)))
  (format t "Memory exported to ~a~%" filename))

(defun pete-visualize (heard recalled reacted spoken)
  (format t "~%=== Pete’s Ultimate Quad Matrix (Convo 1) ===~%")
  (format t ">>> Quadrant 1: Heard (Input Flow) <<<~%")
  (format t "Words: ~a~%" heard)
  (format t "Holes: ~a | Vibe: God’s got this | Source: Input~%"
          (count-if-not (lambda (x) (or (member x *verbs*) (member x *nouns*))) heard))
  (format t "~%>>> Quadrant 2: Recalled (Memory Flip) <<<~%")
  (format t "Words: ~a~%" recalled)
  (format t "Holes: ~a | Vibe: Flipped memory | Source: Memory~%"
          (count-if-not (lambda (x) (or (member x *verbs*) (member x *nouns*))) recalled))
  (format t "~%>>> Quadrant 3: Reacted (Twist Kick) <<<~%")
  (format t "Words: ~a~%" reacted)
  (format t "Holes: ~a | Vibe: Practical twist | Source: Twist~%"
          (count-if-not (lambda (x) (or (member x *verbs*) (member x *nouns*))) reacted))
  (format t "~%>>> Quadrant 4: Spoke (Knowledge Blast) <<<~%")
  (format t "Words: ~a~%" spoken)
  (format t "Holes: ~a | Vibe: God’s got this | Source: Know + Holes~%"
          (count-if-not (lambda (x) (or (member x *verbs*) (member x *nouns*))) spoken)))

(defun pete-visualize-add (heard recalled reacted spoken)
  (format t "~%=== Pete’s Additions Quad Matrix (Convo 2) ===~%")
  (format t ">>> Quadrant 1: Heard 2 (Input Flow) <<<~%")
  (format t "Words: ~a~%" heard)
  (format t "Holes: ~a | Vibe: God’s got this | Source: Input~%"
          (count-if-not (lambda (x) (or (member x *verbs*) (member x *nouns*))) heard))
  (format t "~%>>> Quadrant 2: Recalled 2 (Memory Flip) <<<~%")
  (format t "Words: ~a~%" recalled)
  (format t "Holes: ~a | Vibe: Flipped memory | Source: Memory~%"
          (count-if-not (lambda (x) (or (member x *verbs*) (member x *nouns*))) recalled))
  (format t "~%>>> Quadrant 3: Reacted 2 (Twist Kick) <<<~%")
  (format t "Words: ~a~%" reacted)
  (format t "Holes: ~a | Vibe: Practical twist | Source: Twist~%"
          (count-if-not (lambda (x) (or (member x *verbs*) (member x *nouns*))) reacted))
  (format t "~%>>> Quadrant 4: Spoke 2 (Knowledge Blast) <<<~%")
  (format t "Words: ~a~%" spoken)
  (format t "Holes: ~a | Vibe: God’s got this | Source: Know + Holes~%"
          (count-if-not (lambda (x) (or (member x *verbs*) (member x *nouns*))) spoken)))
          
          
;; Replace the existing pete-treaty (around line ~600)
(defun pete-treaty (candidates holes)
  "Simulates internal deliberation, boosting physics facts for high holes."
  (let* ((options (if (listp candidates) candidates (list candidates)))
         (scored-options (mapcar (lambda (opt)
                                   (let* ((opt-symbols (if (listp opt) opt (list opt)))
                                          (empty-penalty (if (and (eq (car opt-symbols) 'NIL)
                                                                  (eq (cadr opt-symbols) 'MUD))
                                                             0.5 0))
                                          (is-physics (member (prin1-to-string opt) *physics-knowledge* :test #'string=))
                                          (coherence-score (score-coherence opt))
                                          (creativity-score (score-creativity opt))
                                          (responsibility-score (score-responsibility opt holes))
                                          (physics-bonus (if (and is-physics (> holes 5)) 0.3 0)))
                                     (list :option (if (listp opt) opt (list opt))
                                           :score (- (+ (* coherence-score (cdr (assoc :coherence *treaty-principles*)))
                                                        (* creativity-score (cdr (assoc :creativity *treaty-principles*)))
                                                        (* responsibility-score (cdr (assoc :responsibility *treaty-principles*)))
                                                        physics-bonus)
                                                     empty-penalty)
                                           :coherence coherence-score
                                           :creativity creativity-score
                                           :responsibility responsibility-score)))
                                 options))
         (sorted-options (sort scored-options #'> :key (lambda (x) (getf x :score))))
         (best-option (getf (first sorted-options) :option)))
    (format t "~%=== Internal Treaty Deliberation ===~%")
    (dolist (opt sorted-options)
      (format t "Option: ~a | Score: ~a (Coherence: ~a, Creativity: ~a, Responsibility: ~a)~%"
              (getf opt :option) (getf opt :score)
              (getf opt :coherence) (getf opt :creativity) (getf opt :responsibility)))
    (format t "Selected: ~a~%" best-option)
    best-option))

(defun pete-telemetry ()
  (let ((total-holes (reduce #'+ (mapcar (lambda (x)
                                           (count-if-not (lambda (y)
                                                           (or (member y *verbs*) (member y *nouns*)))
                                                         x))
                                         *pete-memory*)))
        (overlap (length (intersection (subseq *pete-memory* 0 (min 5 (length *pete-memory*)))
                                       (subseq *pete-memory* (max 0 (- (length *pete-memory*) 5)))))))
    (format t "~%=== Pete’s Telemetry (3D Overlay) ===~%")
    (format t "Memory Size: ~a | Total Holes: ~a | Convo Overlap: ~a~%"
            (length *pete-memory*) total-holes overlap)))

(defun pete-communicate (input1 input2)
  (format t "~%=== Pete’s Dual Communicating! ===~%")
  (format t ">>> Convo 1: ~a <<<~%" input1)
  (let* ((heard1 (pete-listen input1))
         (recalled1 (pete-read))
         (reacted1 (pete-react recalled1))
         (spoken1 (pete-speak)))
    (format t "~%>>> Convo 2: ~a <<<~%" input2)
    (let* ((heard2 (pete-listen input2))
           (recalled2 (pete-read))
           (reacted2 (pete-react recalled2))
           (spoken2 (pete-speak)))
      (format t "~%Pete’s Dual Loop:~%")
      (format t "Convo 1: Heard ~a, Recalled ~a, Reacted ~a, Spoke ~a~%"
              heard1 recalled1 reacted1 spoken1)
      (format t "Convo 2: Heard ~a, Recalled ~a, Reacted ~a, Spoke ~a~%"
              heard2 recalled2 reacted2 spoken2)
      (pete-visualize heard1 recalled1 reacted1 spoken1)
      (pete-visualize-add heard2 recalled2 reacted2 spoken2)
      (pete-telemetry)
      (push spoken1 *pete-memory*)
      (push spoken2 *pete-memory*)
      (update-memory-graph spoken1)  ; Populate memory graph
      (update-memory-graph spoken2)  ; Populate memory graph
      (when (> (length *pete-memory*) *memory-cap*)
        (setf *pete-memory* (subseq *pete-memory* 0 *memory-cap*)))
      (export-memory-log)
      (list spoken1 spoken2))))

(defun escape-string (s)
  "Escape double quotes for DOT labels."
  (substitute #\\ #\" (princ-to-string s)))

(defun physics-knowledge ()
  "Returns a random three-word physics fact from *physics-knowledge*."
  (let ((snippet (nth (random (length *physics-knowledge*)) *physics-knowledge*)))
    (format t "Pete knows physics: ~a~%" snippet)
    (split-string snippet)))

(defun escape-dot-string (str)
  "Escapes a string for Graphviz DOT syntax by handling quotes, backslashes, and other special characters."
  (let* ((escaped-quotes (cl-ppcre:regex-replace-all "\"" str "\\\""))
         (escaped-backslashes (cl-ppcre:regex-replace-all "\\\\" escaped-quotes "\\\\"))
         (escaped-braces (cl-ppcre:regex-replace-all "[{}]" escaped-backslashes "\\$0"))
         (escaped-brackets (cl-ppcre:regex-replace-all "[\\[\\]]" escaped-braces "\\$0")))
    (cl-ppcre:regex-replace-all "\\\\'" escaped-brackets "'")))

(defun truncate-string (str max-length)
  "Truncates a string to max-length, adding '...' if truncated."
  (if (<= (length str) max-length)
      str
      (concatenate 'string (subseq str 0 max-length) "...")))

(defun flip-hole (hole)
  "Maps a hole (string or symbol) to a subculture-inspired symbol with a 10x amplified literary art vibe."
  (let ((hole-str (if (stringp hole) hole (symbol-name hole)))
        (multiplier 10))
    (case (intern (string-upcase hole-str))
      ;; Los Angeles Subcultures (10x Expanded)
      (ALL 'CRUISE) (AND 'HYDRAULICS) (ANY 'EASTSIDE) (ARE 'CHICANO) (AS 'LOWLOW)
      (AT 'CRUISER) (BE 'PASEO) (BY 'CARHOP) (FOR 'BOUNCE) (FROM 'ZAPATA)
      (HAVE 'HOP) (HE 'CANTINA) (HER 'LOWBOY) (HERE 'PINSTRIPER) (HIM 'CRUISIN)
      (HIS 'CHOLO) (HOW 'DUBS) (I 'TROKA) (IF 'RIM) (IN 'PACAS)
      (IS 'VATO) (IT 'BOMB) (LIKE 'HYDROS) (ME 'CARLO) (MY 'IMPERIAL)
      (NO 'CADDY) (NOT 'IMPALA) (OF 'MONTE) (ON 'TORINO) (ONE 'CUTLASS)
      (OR 'ELDO) (OUT 'REGAL) (SO 'BELAIR) (THAT 'GTO) (THE 'CHEVY)
      (THEIR 'NOVAS) (THEM 'RIVIERA) (THEN 'THUNDERBIRD) (THERE 'CAMARO)
      (THESE 'MUSTANG) (THEY 'CHARGER) (THIS 'CHALLENGER) (TO 'CORVETTE)
      (UP 'GALAXIE) (US 'ROADKING) (WAS 'PONTIAC) (WE 'BUICK) (WHAT 'OLDS)
      (WHEN 'FALCON) (WHERE 'FIREBIRD) (WHICH 'GTO) (WHO 'SKYLAKE) (WHY 'TORONADO)
      ;; Skater Culture (10x Expanded)
      (AFTER 'KICKFLIP) (AGAIN 'GRIND) (ALSO 'OLLIE) (AM 'SHOVE) (AN 'HEELFIP)
      (BEFORE 'NOSEGRIND) (BEING 'TAILSLIDE) (BOTH 'BOARD) (BUT 'DECK) (CAN 'TRUCK)
      (COULD 'WHEELS) (DID 'RAIL) (DO 'POOL) (DOWN 'VERT) (EACH 'RAMP)
      (FROM 'DOGTOWN) (GET 'VENICE) (GO 'ZEPHYR) (HAD 'SKATEPARK) (HAS 'GRIPTAPE)
      (HER 'SHRED) (HIM 'KICKTURN) (HIMSELF 'NOSERIDE) (HIMSELF 'AXLE)
      (HIMSELF 'BUSHING) (HIMSELF 'DUCKWALK) (HIMSELF 'POGO) (HIMSELF 'TIC-TAC)
      (HIMSELF 'MONSTER) (HIMSELF 'BENIHANA) (HIMSELF 'VARIAL) (HIMSELF 'POPSHOVE)
      (HIMSELF 'CASPER) (HIMSELF 'PRIMOSLIDE) (HIMSELF 'HO-HO) (HIMSELF 'SAL-FLAP)
      (HIMSELF 'STALL) (HIMSELF 'ROCK) (HIMSELF 'LIPSLIDE) (HIMSELF 'SMITH)
      (HIMSELF 'FEEBLE) (HIMSELF 'NOLLIE) (HIMSELF 'SWITCH) (HIMSELF 'FAKIE)
      (HIMSELF 'HARD) (HIMSELF 'BLUNT) (HIMSELF 'CROOKED) (HIMSELF '50-50)
      ;; Hip-Hop/Streetwear (10x Expanded)
      (ABOUT 'DRIP) (ABOVE 'SPIT) (ACROSS 'FLOW) (AFTER 'CRASH) (AGAIN 'HUSTLE)
      (AGAINST 'GRIND) (ALONG 'RIDE) (AMONG 'BLADE) (AND 'BLOCK) (ANOTHER 'CAP)
      (ANY 'CREW) (AROUND 'DECK) (AS 'DICE) (AT 'DUB) (BE 'FLAME)
      (BECAUSE 'FRESH) (BEFORE 'GANG) (BEHIND 'HEAT) (BELOW 'ICE) (BENEATH 'JAM)
      (BESIDE 'KICK) (BETWEEN 'LACE) (BEYOND 'MAC) (BUT 'MIC) (BY 'PEACE)
      ;; Global Subcultures (10x Expanded)
      (A 'KAWAII) (ABLE 'HARAJUKU) (ABOUT 'COSPLAY) (ABOVE 'NEON) (ACROSS 'LOLI)
      (AFTER 'YUKATA) (AGAIN 'KOGAL) (AGAINST 'GOTHIC) (ALONG 'KIMONO) (AMONG 'DECORE)
      (AN 'LOLITA) (AND 'KUTSU) (ANOTHER 'PUNK) (ANY 'VISUAL) (AROUND 'FRUITS)
      (AS 'FAIRYKEI) (AT 'MOROI) (BE 'OTOME) (BECAUSE 'SEAPUNK) (BEFORE 'CYBER)
      (BEHIND 'PASTEL) (BELOW 'STEAMPUNK) (BESIDE 'VAPORWAVE) (BETWEEN 'KIREI)
      (BEYOND 'KABUKI) (BUT 'J-FASHION) (BY 'ANIME) (CALL 'RAVE) (CAN 'TECHNO)
      (COULD 'BERGHAIN) (DID 'BEAT) (DO 'VOID) (DOWN 'DROP) (DURING 'MIX)
      (EACH 'KICK) (FROM 'GRIME) (GET 'SPRAY) (GO 'ROADMAN) (HAD 'ENDS)
      (HAS 'BRUV) (HAVE 'DUBSTEP) (HE 'GARAGE) (HER 'BASS) (HIM 'JUNGLE)
      (HIMSELF 'UKG) (HIMSELF 'WOBBLE) (HIMSELF 'RINSE) (HIMSELF 'CREW)
      (HIMSELF 'MC) (HIMSELF 'BARS) (HIMSELF 'MANDO) (HIMSELF 'TING)
      (HIMSELF 'YARDIE) (HIMSELF 'SKENG) (HIMSELF 'WASTEMAN) (HIMSELF 'LINK)
      (HIMSELF 'GREEZE) (HIMSELF 'PENG) (HIMSELF 'CHING) (HIMSELF 'FAM)
      ;; Generic Literary Art (10x Expanded)
      (IF 'RHYTHM) (IN 'SPARK) (INTO 'VIBE) (ITS 'VERSE) (MORE 'SONG)
      (MOST 'POEM) (MUST 'STANZA) (MY 'RHYME) (NEAR 'IMAGERY) (NEED 'METAPHOR)
      (NEVER 'SYMBOL) (NEXT 'ALLEGORY) (NO 'ELEGY) (NOR 'ODE) (NOT 'HAIKU)
      (NOW 'SONNET) (OF 'EPIC) (OFF 'BALLAD) (ON 'LYRIC) (ONCE 'QUATRAIN)
      (ONLY 'COUPLET) (OR 'TERCET) (OTHER 'SESTET) (OUR 'OCTAVE) (OUT 'FREEFORM)
      (OVER 'CANTO) (PER 'REFRAIN) (SAME 'STROPHE) (SAY 'CADENCE) (SHE 'TONE)
      (SHOULD 'METER) (SINCE 'RHYTHM) (SO 'ALLITERATION) (SOME 'ASSONANCE)
      (SUCH 'CONSONANCE) (THAN 'ENJAMBMENT) (THAT 'CAESURA) (THEIR 'FOOT)
      (THEM 'SCANSION) (THEN 'IAMB) (THERE 'TROCHEE) (THESE 'DACTYL) (THEY 'ANAPEST)
      (THING 'SPONDEE) (THIS 'PYRRHIC) (THOSE 'AMPHIBRACH) (THOUGH 'CATALEXIS)
      (THROUGH 'CHORIAMB) (TILL 'MOLOSSUS) (TO 'BACCHAEUS) (TOO 'CRETICT)
      (UNDER 'ANTISPAST) (UNTIL 'CHOREUS) (UPON 'IONIC) (US 'PAEON)
      (VERY 'EPITRITE) (WAS 'DISPONDEE) (WAY 'PROCELEUSMATIC) (WE 'DITROCHEE)
      (WELL 'TRIBRACH) (WERE 'AMPHIMACER) (WHAT 'ANTIBACCHIUS) (WHEN 'PHERECRATEAN)
      (WHENEVER 'ADONIC) (WHERE 'GLYCONIC) (WHICH 'PHALACEAN) (WHILE 'ASCLEPIAD)
      (WHO 'SAPPHIC) (WHOM 'ALCAIC) (WHY 'HENDECASYLLABIC) (WILL 'ARCHILOCHIAN)
      (WITH 'TELESTICH) (WITHIN 'ACROSTIC) (WITHOUT 'MESOSTICH) (WORK 'SYLLABIC)
      (WOULD 'ACCENTUAL) (YET 'QUANTITATIVE)
      (otherwise (let ((base-options '(CRUISE GRIND DRIP KAWII RAVE GRIME SHRED NEON BEAT FLOW RHYTHM SPARK VIBE)))
                   (nth (random (* multiplier (length base-options))) base-options))))))
                   
                   
(defun compute-transformation-info (match)
  "Computes transformation probabilities and possible paths for a memory entry."
  (let* ((holes (count-if-not (lambda (x)
                                (or (member x *verbs*) (member x *nouns*)))
                              (alexandria:flatten match)))
         (p1 (- 0.5 (* 0.05 holes)))
         (p2 (+ 0.25 (* 0.025 holes)))
         (p3 (+ 0.25 (* 0.025 holes)))
         (normalized-p1 (max 0.1 p1))
         (normalized-p2 (min 0.45 p2))
         (normalized-p3 (min 0.45 p3))
         (paths (cond
                  ((member 'rudders match)
                   '((dreams spark fire) (waves crash cliffs) (thunder shakes ground)))
                  ((member "dreams spark fire" match :test #'equal)
                   '((bells toll end) (light bends shadows) (frost bites air)))
                  ((member "bells toll end" match :test #'equal)
                   '((mem_5l mud) (stars guide night) (ice locks rivers)))
                  (t nil))))
    (list :probabilities (format nil "Probabilities: [Stable: ~a, Moderate: ~a, Chaotic: ~a]"
                                 normalized-p1 normalized-p2 normalized-p3)
          :paths (if paths
                     (format nil "Paths: ~a | ~a | ~a"
                             (prin1-to-string (first paths))
                             (prin1-to-string (second paths))
                             (prin1-to-string (third paths)))
                     "Paths: None"))))

(defun neural-diagram ()
  "Generates a neural diagram with a Technical Engineering aesthetic, flipped values, and attractive styling for bystanders (no gradients)."
  (let ((dot-file "neural-diagram.dot")
        (output-file "neural-diagram.png")
        ;; Filter *pete-memory* to ensure all entries are lists of symbols
        (filtered-memory (remove-if (lambda (x) (not (every #'symbolp x))) *pete-memory*)))
    (with-open-file (stream dot-file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format stream "digraph NeuralDiagram {~%")
      (format stream "  rankdir=BT;~%")
      (format stream "  bgcolor=\"white\";~%")
      (format stream "  label=\"Pete's Neural Diagram: Quantum Hole Flow\\nConversational Memory with Subculture Vibes\";~%")
      (format stream "  labelloc=\"t\";~%")
      (format stream "  nodesep=0.5;~%") ; Increase separation between nodes at the same rank
      (format stream "  ranksep=1.0;~%") ; Increase separation between ranks
      (format stream "  splines=ortho;~%") ; Use orthogonal edges to reduce clutter
      (format stream "  node [style=filled fontcolor=black fontsize=10 fontname=\"Helvetica\" shape=ellipse];~%")
      (format stream "  edge [color=navy fontsize=8 fontcolor=black fontname=\"Helvetica\"];~%")
      (format stream "  compound=true;~%")

      ;; Define subgraphs with Technical Engineering colors (no gradients)
      (format stream "  subgraph cluster_lowrider {~%    style=filled;~%    color=steelblue;~%    fillcolor=\"steelblue\";~%    label=\"Lowrider Cluster\";~%  }~%")
      (format stream "  subgraph cluster_skater {~%    style=filled;~%    color=steelblue;~%    fillcolor=\"steelblue\";~%    label=\"Skater Cluster\";~%  }~%")
      (format stream "  subgraph cluster_hiphop {~%    style=filled;~%    color=steelblue;~%    fillcolor=\"steelblue\";~%    label=\"Hip-Hop Cluster\";~%  }~%")
      (format stream "  subgraph cluster_harajuku {~%    style=filled;~%    color=steelblue;~%    fillcolor=\"steelblue\";~%    label=\"Harajuku Cluster\";~%  }~%")
      (format stream "  subgraph cluster_grime {~%    style=filled;~%    color=slategray;~%    fillcolor=\"slategray\";~%    label=\"Grime Cluster\";~%  }~%")
      (format stream "  subgraph cluster_techno {~%    style=filled;~%    color=cornflowerblue;~%    fillcolor=\"cornflowerblue\";~%    label=\"Techno Cluster\";~%  }~%")
      (format stream "  subgraph cluster_literary {~%    style=filled;~%    color=darkgray;~%    fillcolor=\"darkgray\";~%    label=\"Literary Cluster\";~%  }~%")
      (format stream "  subgraph cluster_default {~%    style=filled;~%    color=lightgray;~%    fillcolor=\"lightgray\";~%    label=\"Default Cluster\";~%  }~%")

      ;; Define a legend subgraph
      (format stream "  subgraph cluster_legend {~%")
      (format stream "    style=filled;~%    fillcolor=lightgray;~%    label=\"Legend\";~%")
      (format stream "    \"legend_harajuku\" [label=\"Harajuku: Cultural Spark\" fillcolor=\"steelblue\" shape=box];~%")
      (format stream "    \"legend_techno\" [label=\"Techno: Beat Pulse\" fillcolor=\"cornflowerblue\" shape=box];~%")
      (format stream "    \"legend_literary\" [label=\"Literary: Poetic Flow\" fillcolor=\"darkgray\" shape=box];~%")
      (format stream "    \"legend_default\" [label=\"Default: Neutral Flow\" fillcolor=\"lightgray\" shape=box];~%")
      (format stream "  }~%")

      ;; Define subculture and literary symbol lists
      (let ((lowrider-symbols '(CRUISE HYDRAULICS EASTSIDE CHICANO LOWLOW CRUISER PASEO CARHOP BOUNCE ZAPATA
                                HOP CANTINA LOWBOY PINSTRIPER CRUISIN CHOLO DUBS TROKA RIM PACAS
                                VATO BOMB HYDROS CARLO IMPERIAL CADDY IMPALA MONTE TORINO CUTLASS
                                ELDO REGAL BELAIR GTO CHEVY NOVAS RIVIERA THUNDERBIRD CAMARO
                                MUSTANG CHARGER CHALLENGER CORVETTE GALAXIE ROADKING PONTIAC BUICK OLDS
                                FALCON FIREBIRD GTO SKYLAKE TORONADO))
            (skater-symbols '(KICKFLIP GRIND OLLIE SHOVE HEELFIP NOSEGRIND TAILSLIDE BOARD DECK TRUCK
                              WHEELS RAIL POOL VERT RAMP DOGTOWN VENICE ZEPHYR SKATEPARK GRIPTAPE
                              SHRED KICKTURN NOSERIDE AXLE BUSHING DUCKWALK POGO TIC-TAC MONSTER
                              BENIHANA VARIAL POPSHOVE CASPER PRIMOSLIDE HO-HO SAL-FLAP STALL
                              ROCK LIPSLIDE SMITH FEEBLE NOLLIE SWITCH FAKIE HARD BLUNT CROOKED |50-50|))
            (hiphop-symbols '(DRIP SPIT FLOW CRASH HUSTLE GRIND RIDE BLADE BLOCK CAP
                              CREW DECK DICE DUB FLAME FRESH GANG HEAT ICE JAM
                              KICK LACE MAC MIC PEACE))
            (harajuku-symbols '(KAWAII HARAJUKU COSPLAY NEON LOLI YUKATA KOGAL GOTHIC KIMONO DECORE
                               LOLITA KUTSU PUNK VISUAL FRUITS FAIRYKEI MOROI OTOME SEAPUNK CYBER
                               PASTEL STEAMPUNK VAPORWAVE KIREI KABUKI J-FASHION ANIME))
            (grime-symbols '(GRIME SPRAY ROADMAN ENDS BRUV DUBSTEP GARAGE BASS JUNGLE UKG
                             WOBBLE RINSE CREW MC BARS MANDO TING YARDIE SKENG WASTEMAN
                             LINK GREEZE PENG CHING FAM))
            (techno-symbols '(RAVE TECHNO BERGHAIN BEAT VOID DROP MIX KICK))
            (literary-symbols '(RHYTHM SPARK VIBE VERSE SONG POEM STANZA RHYME IMAGERY METAPHOR
                                SYMBOL ALLEGORY ELEGY ODE HAIKU SONNET EPIC BALLAD LYRIC QUATRAIN
                                COUPLET TERCET SESTET OCTAVE FREEFORM CANTO REFRAIN STROPHE CADENCE
                                TONE METER ALLITERATION ASSONANCE CONSONANCE ENJAMBMENT CAESURA FOOT
                                SCANSION IAMB TROCHEE DACTYL ANAPEST SPONDEE PYRRHIC AMPHIBRACH
                                CATALEXIS CHORIAMB MOLOSSUS BACCHAEUS CRETICT ANTISPAST CHOREUS
                                IONIC PAEON EPITRITE DISPONDEE PROCELEUSMATIC DITROCHEE TRIBRACH
                                AMPHIMACER ANTIBACCHIUS PHERECRATEAN ADONIC GLYCONIC PHALACEAN
                                ASCLEPIAD SAPPHIC ALCAIC HENDECASYLLABIC ARCHILOCHIAN TELESTICH
                                ACROSTIC MESOSTICH SYLLABIC ACCENTUAL QUANTITATIVE)))

        ;; Process memory entries as nodes within their respective subgraphs
        (if (not filtered-memory)
            (format stream "  \"Empty\" [label=\"No Memory\" fillcolor=\"lightgray\"];~%")
            (loop for entry in filtered-memory
                  for idx from 0
                  do (let* ((holes (count-if-not (lambda (x)
                                                   (or (member x *verbs*) (member x *nouns*)))
                                                 (if (listp entry) entry (list entry))))
                            (flipped (mapcar (lambda (x)
                                               (if (or (member x *verbs*) (member x *nouns*))
                                                   x
                                                   (flip-hole (if (stringp x) x (symbol-name x)))))
                                             (if (listp entry) entry (list entry))))
                            (subculture (cond
                                          ((some (lambda (x) (member x lowrider-symbols)) flipped) 'lowrider)
                                          ((some (lambda (x) (member x skater-symbols)) flipped) 'skater)
                                          ((some (lambda (x) (member x hiphop-symbols)) flipped) 'hiphop)
                                          ((some (lambda (x) (member x harajuku-symbols)) flipped) 'harajuku)
                                          ((some (lambda (x) (member x grime-symbols)) flipped) 'grime)
                                          ((some (lambda (x) (member x techno-symbols)) flipped) 'techno)
                                          ((some (lambda (x) (member x literary-symbols)) flipped) 'literary)
                                          (t 'default)))
                            (treaty-marker (if (member 'treaty entry) "[Treaty]" ""))
                            (trans-info (compute-transformation-info entry))
                            (label (format nil "Mem_~a\\n~a\\nHoles: ~a~a\\nFlipped: ~a\\n[fontsize=8]~a\\n~a"
                                           idx
                                           (truncate-string (escape-dot-string (prin1-to-string entry)) 50)
                                           holes
                                           treaty-marker
                                           (truncate-string (escape-dot-string (prin1-to-string flipped)) 50)
                                           (getf trans-info :probabilities)
                                           (truncate-string (getf trans-info :paths) 30)))
                            (fillcolor (case subculture
                                         (lowrider "steelblue")
                                         (skater "steelblue")
                                         (hiphop "steelblue")
                                         (harajuku "steelblue")
                                         (grime "slategray")
                                         (techno "cornflowerblue")
                                         (literary "darkgray")
                                         (t "lightgray"))))
                       (format stream "  subgraph cluster_~a {~%    \"Mem_~a\" [label=\"~a\" fillcolor=~a];~%  }~%"
                               (symbol-name subculture) idx label fillcolor)))))

      ;; Define edges with Technical Engineering styling
      (loop for i from 0 below (1- (length filtered-memory))
            do (let* ((node1 (nth i filtered-memory))
                      (node2 (nth (1+ i) filtered-memory))
                      (holes1 (count-if-not (lambda (x)
                                              (or (member x *verbs*) (member x *nouns*)))
                                            (if (listp node1) node1 (list node1))))
                      (holes2 (count-if-not (lambda (x)
                                              (or (member x *verbs*) (member x *nouns*)))
                                            (if (listp node2) node2 (list node2))))
                      (avg-holes (/ (+ holes1 holes2) 2.0))
                      (penwidth (max 1 (min 3 (floor (* avg-holes 0.3)))))
                      (style (cond ((> avg-holes 5) "dotted")
                                   (t "solid"))))
                 (format stream "  \"Mem_~a\" -> \"Mem_~a\" [label=\"flow\" penwidth=~a style=~a];~%" i (1+ i) penwidth style)))

      (format stream "}~%"))
    (handler-case
        (progn
          (uiop:run-program (format nil "dot -Tpng ~a -o ~a" dot-file output-file)
                            :output t :error-output t)
          (format t "Technical Engineering neural diagram with flipped values (attractive, no gradients) DOT file written to ~a~%" dot-file)
          (format t "Graph rendered to ~a~%" output-file))
      (error (e)
        (format t "Error rendering graph: ~a. Ensure Graphviz is installed.~%" e))))
  (values))
  
(defun clear-memory ()
  "Clears Pete's memory to start fresh."
  (setf *pete-memory* '())
  (clrhash *pete-memory-graph*)
  (format t "Pete's memory cleared! Starting fresh.~%"))

(defun converse-no-reset ()
  "Starts PeteAI without resetting memory."
  (format t "~%=== Pete’s Ready! Say stuff (or 'quit', 'know', 'speak', 'listen', 'read', 'react', 'communicate', 'export', 'neural-diagram', 'clear-memory') ===~%")
  (finish-output)
  (loop
    (format t "> ")
    (finish-output)
    (let ((input (read-line)))
      (cond
        ((string-equal input "quit")
         (when *pete-memory*
           (let ((last-thought (nth (random (length *pete-memory*)) *pete-memory*)))
             (format t "Pete sums it: Twisted ~a into wild vibes!~%" last-thought)))
         (format t "Pete waves: See ya!~%")
         (finish-output)
         nil)
        ((string-equal input "know") (local-knowledge))
        ((string-equal input "speak") (pete-speak))
        ((string-equal input "listen") (pete-listen (read-line)))
        ((string-equal input "read") (pete-read))
        ((string-equal input "react")
         (pete-react (if *pete-memory* (car *pete-memory*) '(pete vibes))))
        ((string-equal input "communicate")
         (format t "Enter first convo: ") (finish-output)
         (let ((input1 (read-line)))
           (format t "Enter second convo: ") (finish-output)
           (let ((input2 (read-line)))
             (pete-communicate input1 input2))))
        ((string-equal input "export")
         (export-memory-log "pete_AI_beast_memory.txt"))
        ((string-equal input "neural-diagram")
         (neural-diagram))
        ((string-equal input "clear-memory")
         (clear-memory))
        (t
         (multiple-value-bind (untagged holes) (split-and-tag input)
           (format t "~%")
           (let ((result (pete-flow untagged 0 holes)))
             (format t "~%Pete: ~a~%" result))))))))


