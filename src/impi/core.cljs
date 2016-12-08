(ns impi.core
  (:require cljsjs.pixi))

(def log js/console.log)

(defn- update-count [child f]
  (set! (.-impiCount child) (f (.-impiCount child))))

(defn- replace-child [container child i]
  (let [old-child (aget (.-children container) i)]
    (when-not (identical? child old-child)
      (aset (.-children container) i child)
      (update-count old-child (fnil dec 1))
      (update-count child (fnil inc 0))
      old-child)))

(defn- append-child [container child]
  (.push (.-children container) child)
  (update-count child (fnil inc 0)))

(defn- overwrite-children [container children]
  (let [length (-> container .-children .-length)]
    (loop [i 0, children children, replaced ()]
      (if (seq children)
        (let [child (first children)]
          (if (< i length)
            (if-let [old-child (replace-child container child i)]
              (recur (inc i) (rest children) (cons old-child replaced))
              (recur (inc i) (rest children) replaced))
            (do (append-child container child)
                (recur (inc i) (rest children) replaced))))
        replaced))))

(defn- trim-children [container index]
  (let [children (.-children container)]
    (if (< index (.-length children))
      (let [removed (.slice children index)]
        (.splice children index)
        removed))))

(defn- set-parent [child parent]
  (set! (.-parent child) parent)
  (js-delete child "impiCount"))

(defn- clear-parent [child]
  (when (zero? (.-impiCount child))
    (set-parent child nil)))

; Slow
(defn- replace-children [container children]
  (let [length   (-> container .-children .-length)
        replaced (overwrite-children container children)
        removed  (trim-children container (count children))
        changed? (or (seq replaced) (not= length (-> container .-children .-length)))]
    (when changed?
      (run! clear-parent replaced)
      (run! clear-parent removed)
      (run! #(set-parent % container) (.-children container)))))

(def listeners (atom {}))

(defn- replace-listener [object event index [key & args]]
  (let [listener ((@listeners (first index)) key)]
    (doto object
      (.removeAllListeners event)
      (.on event #(apply listener % args)))))

(defn- rectangle [[x y w h]]
  (js/PIXI.Rectangle. x y w h))

(defn- image [src]
  (let [image (js/Image.)]
    (set! (.-src image) src)
    image))

(def ^:private scale-modes
  {:pixi.texture.scale-mode/linear  js/PIXI.SCALE_MODES.LINEAR
   :pixi.texture.scale-mode/nearest js/PIXI.SCALE_MODES.NEAREST})

(def ^:private blend-modes
  {:pixi.object.blend-mode/normal   js/PIXI.BLEND_MODES.NORMAL
   :pixi.object.blend-mode/add      js/PIXI.BLEND_MODES.ADD
   :pixi.object.blend-mode/multiply js/PIXI.BLEND_MODES.MULTIPLY
   :pixi.object.blend-mode/screen   js/PIXI.BLEND_MODES.SCREEN})

(def ^:private text-properties
  {:pixi.text.style/align "align"
   :pixi.text.style/break-words "breakWords"
   :pixi.text.style/drop-shadow "dropShadow"
   :pixi.text.style/drop-shadow-angle "dropShadowAngle"
   :pixi.text.style/drop-shadow-blur "dropShadowBlur"
   :pixi.text.style/drop-shadow-color "dropShadowSolor"
   :pixi.text.style/drop-shadow-distance "dropShadowDistance"
   :pixi.text.style/fill "fill"
   :pixi.text.style/font-family "fontFamily"
   :pixi.text.style/font-size "fontSize"
   :pixi.text.style/font-style "fontStyle"
   :pixi.text.style/font-variant "fontVariant"
   :pixi.text.style/font-weight "fontWeight"
   :pixi.text.style/letter-spacing "letterSpacing"
   :pixi.text.style/line-height "lineHeight"
   :pixi.text.style/line-join "lineJoin"
   :pixi.text.style/miter-limit "miterLimit"
   :pixi.text.style/padding "padding"
   :pixi.text.style/stroke "stroke"
   :pixi.text.style/stroke-thickness "strokeThickness"
   :pixi.text.style/word-wrap "wordWrap"
   :pixi.text.style/word-wrap-width "wordWrapWidth"})

(def base-texture-cache    (atom {}))
(def pending-base-textures (atom #{}))

(defn- base-texture-key [texture]
  [(:pixi.texture/source texture)
   (:pixi.texture/scale-mode texture)])

(defn- create-base-texture [texture]
  (let [source (texture :pixi.texture/source)
        mode   (-> texture :pixi.texture/scale-mode scale-modes)]
    (js/PIXI.BaseTexture. 
      (cond (string? source) (image source) 
            true source))))

(defn- get-base-texture [texture]
  (let [key (base-texture-key texture)]
    (or (@base-texture-cache key)
        (let [object (create-base-texture texture)]
          (swap! base-texture-cache assoc key object)
          (swap! pending-base-textures conj object)
          (.on object "loaded" #(swap! pending-base-textures disj object))
          object))))

(defn- on-loaded-textures [f]
  (doseq [texture @pending-base-textures]
    (.on texture "loaded" f)))

(def texture-cache (atom {}))

(defn- create-texture [texture]
  (js/PIXI.Texture.
   (get-base-texture texture)
   (some-> texture :pixi.texture/frame rectangle)
   (some-> texture :pixi.texture/crop rectangle)
   (some-> texture :pixi.texture/trim rectangle)
   (some-> texture :pixi.texture/rotate?)))

(defn- get-texture [texture]
  (or (@texture-cache texture)
      (let [object (create-texture texture)]
        (swap! texture-cache assoc texture object)
        object)))

(defn- create-render-texture [texture]
  (let [mode  (-> texture :pixi.texture/scale-mode scale-modes)
        [w h] (:pixi.render-texture/size texture)]
    (.create js/PIXI.RenderTexture w h mode)))

(defmulti draw-shape! (fn [_ v] (v :pixi.shape/type)))

(defmethod draw-shape! :pixi.shape.type/circle 
  [graphics {[x y]  :pixi.shape/position
             radius :pixi.circle/radius}]
  (.drawCircle graphics x y radius))

(defmethod draw-shape! :pixi.shape.type/ellipse 
  [graphics {[x y]               :pixi.shape/position
             [x-radius y-radius] :pixi.ellipse/radius}]
  (.drawEllipse graphics x y x-radius y-radius))

(defmethod draw-shape! :pixi.shape.type/polygon 
  [graphics {path :pixi.polygon/path}]
  (if (= (type path) cljs.core/PersistentVector)
    (.drawPolygon graphics (clj->js path))
    (.drawPolygon graphics path)))

(defmethod draw-shape! :pixi.shape.type/rectangle
  [graphics {[x y]          :pixi.shape/position
             [width height] :pixi.shape/size}]
  (.drawRect graphics x y width height))

(defmethod draw-shape! :pixi.shape.type/rounded-rectangle
  [graphics {[x y]          :pixi.shape/position
             [width height] :pixi.shape/size
             radius         :pixi.rounded-rectangle/radius}]
  (.drawRoundedRect graphics x y width height radius))

(defmulti create-filter :pixi.filter/type)

(defmethod create-filter :pixi.filter.type/fxaa-filter
  [filter]
  (js/PIXI.filters.FXAAFilter.))

(defmethod create-filter :pixi.filter.type/drop-shadow-filter
  [filter]
  (js/PIXI.filters.DropShadowFilter.))

(defmethod create-filter :pixi.filter.type/blur-filter
  [filter]
  (js/PIXI.filters.BlurFilter.))

(defmethod create-filter :pixi.filter.type/color-matrix-filter
  [filter]
  (js/PIXI.filters.ColorMatrixFilter.))

(defmethod create-filter nil
  [filter]
  (js/PIXI.Filter.
   (:pixi.filter/vertex filter)
   (:pixi.filter/fragment filter)
   (clj->js (:pixi.filter/uniforms filter))))

(declare build!)

(declare ^:dynamic *renderer*)

(defmulti create-object :pixi.object/type)

(defmethod create-object :pixi.object.type/sprite [_]
  {:val {}, :obj (js/PIXI.Sprite.)})

(defmethod create-object :pixi.object.type/container [_]
  {:val {}, :obj (js/PIXI.Container.)})

(defmethod create-object :pixi.object.type/graphics [_]
  {:val {}, :obj (js/PIXI.Graphics.)})

(defmethod create-object :pixi.object.type/text [_]
  {:val {}, :obj (js/PIXI.Text.)})

(defmulti create
  (fn [attr value] attr))

(def ^:private renderer-arguments  
  [:pixi.renderer/size
   :pixi.renderer/antialias?
   :pixi.renderer/transparent?
   :pixi.renderer/no-webgl?
   :pixi.renderer/resolution])

(defmethod create :pixi/renderer
  [_ {[w h] :pixi.renderer/size
      :keys [pixi.renderer/transparent? pixi.renderer/antialias?
             pixi.renderer/no-webgl?    pixi.renderer/resolution]
      :as options}]
  {:val (select-keys options renderer-arguments)
   :obj (js/PIXI.autoDetectRenderer w h #js {:transparent transparent?
                                             :antialias   antialias?
                                             :resolution  (or resolution 1)}
                                    no-webgl?)})

(defmethod create :pixi/stage [_ value]
  (create-object value))

(defmethod create :pixi.container/children [_ value]
  (create-object value))

(defmethod create :pixi.render-texture/source [_ value]
  (create-object value))

(defmethod create :pixi.sprite/texture [_ value]
  (if (contains? value :pixi.render-texture/source)
    {:val (dissoc value :pixi.render-texture/source)
     :obj (create-render-texture value)}
    {:val value, :obj (get-texture value)}))

(defmethod create :pixi.graphics/shapes [graphics value]
  {:val {}, :obj #js {}})

(defmethod create :pixi.text/style [_ value]
  {:val {}, :obj (js/PIXI.TextStyle.)})

(defmethod create :pixi.object/filters [_ value]
  {:val {}, :obj (create-filter value)})

(defmulti update-prop! (fn [object index attr value]  attr))

(defmethod update-prop! :default [object _ attr _]
  ; (print :nil attr)
  )

(defmethod update-prop! :pixi.object/alpha [object _ _ alpha]
  (set! (.-alpha object) (or alpha 1.0)))

(defmethod update-prop! :pixi.object/blend-mode [object _ _ mode]
  (set! (.-blendMode object) (blend-modes mode js/PIXI.BLEND_MODES.NORMAL)))

(defmethod update-prop! :pixi.object/position [object _ _ [x y]]
  (set! (-> object .-position .-x) x)
  (set! (-> object .-position .-y) y))

(defmethod update-prop! :pixi.object/rotation [object _ _ angle]
  (set! (.-rotation object) angle))

(defmethod update-prop! :pixi.object/scale [object _ _ [x y]]
  (set! (-> object .-scale .-x) x)
  (set! (-> object .-scale .-y) y))

(defmethod update-prop! :pixi.object/filters [object index attr filters]
  (set! (.-filters object) (apply array (map #(build! index attr %) filters))))

(defmethod update-prop! :pixi.blur-filter/blur  [object _ _ blur]
  (set! (.-blur object) blur))

(defmethod update-prop! :pixi.blur-filter/blur-x  [object _ _ blur]
  (set! (.-blurX object) blur))

(defmethod update-prop! :pixi.blur-filter/blur-y  [object _ _ blur]
  (set! (.-blurY object) blur))

(defmethod update-prop! :pixi.drop-shadow-filter/blur  [object _ _ blur]
  (set! (.-blur object) blur))

(defmethod update-prop! :pixi.drop-shadow-filter/color  [object _ _ color]
  (set! (.-color object) color))

(defmethod update-prop! :pixi.drop-shadow-filter/angle  [object _ _ angle]
  (set! (.-angle object) angle))

(defmethod update-prop! :pixi.drop-shadow-filter/distance  [object _ _ distance]
  (set! (.-distance object) distance))

(defmethod update-prop! :pixi.drop-shadow-filter/alpha  [object _ _ alpha]
  (set! (.-alpha object) alpha))

(defmethod update-prop! :pixi.color-matrix-filter/distance  [object _ _ matrix]
  (set! (.-matrix object) matrix))

(defmethod update-prop! :pixi.object/interactive? [object _ _ interactive?]
  (set! (.-interactive object) interactive?))

(defmethod update-prop! :pixi.object/hit-area [object _ _ rect]
  (if (some? rect)
    (set! (.-hitArea object) (rectangle rect))
    (js-delete object "hitArea")))

(defmethod update-prop! :pixi.object/contains-point [object _ _ pred]
  (if (some? pred)
    (set! (.-containsPoint object) pred)
    (js-delete object "containsPoint")))

(def event-properties
  {:pixi.event/click           "click"
   :pixi.event/mousemove       "mousemove"
   :pixi.event/mouseout        "mouseout"
   :pixi.event/mouseover       "mouseover"
   :pixi.event/mouseup         "mouseup"
   :pixi.event/mouseupoutside  "mouseupoutside"
   :pixi.event/rightclick      "rightclick"
   :pixi.event/rightdown       "rightdown"
   :pixi.event/rightup         "rightup"
   :pixi.event/rightupoutside  "rightupoutside"
   :pixi.event/tap             "tap"
   :pixi.event/touchend        "touchend"
   :pixi.event/touchendoutside "touchendoutside"
   :pixi.event/touchmove       "touchmove"
   :pixi.event/touchstart      "touchstart"})

(doseq [attr (keys event-properties)]
  (derive attr :pixi/event))

(defmethod update-prop! :pixi.event [object index event listener]
  (replace-listener object (event-properties event) index listener))

(defmethod update-prop! :pixi.container/children [container index attr children]
  (->> (if (map? children) (vals children) children)
       (map #(build! index attr %))
       (doall)
       (replace-children container)))

(defmethod update-prop! :pixi.container/size [container index attr [w h]]
  (set! (.-width container) w)
  (set! (.-height container) h))

(defmethod update-prop! :pixi.graphics/shapes 
  [graphics-obj _ _ shapes]
  (.clear graphics-obj)
  (doseq [{:keys [pixi.shape/fill pixi.shape/line pixi.shape/shadow] :as shape}
          (if (map? shapes) (vals shapes) shapes)]
    (.lineStyle graphics-obj
                (or (:pixi.line/width line) 0)
                (:pixi.line/color line)
                (or (:pixi.line/alpha line) 1))
    (set! (.-shadowColor graphics-obj)   (:pixi.shadow/color shadow))
    (set! (.-shadowBlur graphics-obj)    (:pixi.shadow/blur shadow))
    (set! (.-shadowOffsetX graphics-obj) (some-> shadow :pixi.shadow/offset first))
    (set! (.-shadowOffsetY graphics-obj) (some-> shadow :pixi.shadow/offset second))
    (when fill
      (.beginFill graphics-obj
                  (:pixi.fill/color fill)
                  (or (:pixi.fill/alpha fill) 1)))
    (draw-shape! graphics-obj shape)
    (when fill (.endFill graphics-obj))))

(defmethod update-prop! :pixi.text/text  [object _ _ text]
  (set! (.-text object) text))

(defmethod update-prop! :pixi.text/style [object index attr text-style]
  (set! (.-style object) (build! index attr text-style)))

(doseq [attr (keys text-properties)]
  (derive attr :pixi.text.style/property))

(defmethod update-prop! :pixi.text.style/property [object _ attr value]
  (aset object (text-properties attr) value))

(defmethod update-prop! :pixi.sprite/anchor [sprite _ _ [x y]]
  (set! (-> sprite .-anchor .-x) x)
  (set! (-> sprite .-anchor .-y) y))

(defmethod update-prop! :pixi.object/pivot [sprite _ _ [x y]]
  (set! (-> sprite .-pivot .-x) x)
  (set! (-> sprite .-pivot .-y) y))

(defmethod update-prop! :pixi.sprite/texture [sprite index attr texture]
  (set! (.-texture sprite) (build! index attr texture)))

(defmethod update-prop! :pixi.render-texture/source [texture index attr scene]
  (let [source   (build! index attr scene)
        renderer *renderer*]
    (.render renderer source texture)
    (on-loaded-textures #(.render renderer source texture))))

(defmethod update-prop! :pixi.render-texture/size [texture _ _ [w h]]
  (.resize texture w h true))

(defmethod update-prop! :pixi.renderer/size [renderer _ _ [w h]]
  (.resize renderer w h))

(defmethod update-prop! :pixi.renderer/background-color [renderer _ _ color]
  (set! (.-backgroundColor renderer) color))

(defmethod update-prop! :pixi.filter/padding [object _ _ padding]
  (set! (.-padding object) padding))

(defn- run-kv! [proc m]
  (reduce-kv (fn [_ k v] (proc k v) nil) nil m)
  nil)

(defmulti prop-not-changed?  (fn [k v ov] k))

(defmethod prop-not-changed? :pixi.container/children  [_ v ov]
  (identical? v ov))

(defmethod prop-not-changed? :pixi.graphics/shapes  [_ v ov]
  (identical? v ov))

(defmethod prop-not-changed? :default  [_ v ov]
  (= v ov))

(defn- update-changed-prop! [object index old-value k v]
  (when-not (prop-not-changed? k v (old-value k)) (update-prop! object index k v)))

(defn- update-removed-prop! [object index new-value k]
  (when-not (contains? new-value k) (update-prop! object index k nil)))

(defn- update! [{object :obj old-value :val} index attr new-value]
  (run-kv! (fn [k v] (update-changed-prop! object index old-value k v)) new-value)
  (run-kv! (fn [k _] (update-removed-prop! object index new-value k)) old-value)
  {:val new-value, :obj object})

(def recreate-keys
  #{:pixi.renderer/antialias?
    :pixi.renderer/transparent?
    :pixi.renderer/no-webgl?
    :pixi.renderer/resolution
    :pixi.object/type
    :pixi.texture/scale-mode
    :pixi.texture/source
    :pixi.texture/frame
    :pixi.texture/crop
    :pixi.texture/trim
    :pixi.texture/rotate
    :pixi.filter/vertex
    :pixi.filter/fragment
    :pixi.filter/uniforms})

(defn- should-recreate? [old-value new-value]
  false)

(def build-cache (atom {}))

(defn- build! [index attr value]
  (let [key    (:impi/key value)
        index  (or (:impi.key/shared value) (conj index attr key))
        cache! #(swap! build-cache assoc index %)]
    (:obj (if-let [cached (@build-cache index)]
            (let [cached-val (:val cached)]
              (if (identical? value cached-val)
                cached
                (do ;(print :t (value :pixi.object/type))
                    (-> (if (should-recreate? cached-val value)
                      (create attr value)
                      cached)
                    (update! index attr value)
                    (doto cache!)))))
            (-> (create attr value)
                (update! index attr value)
                (doto cache!))))))

(defn- renderer-mounted? [renderer element]
  (identical? element (.. renderer -view -parentNode)))

(defn- mount-view [renderer element]
  (when-not (renderer-mounted? renderer element)
    (set! (.-innerHTML element) "")
    (.appendChild element (.-view renderer))))

(defn- build-listeners! [key scene]
  (swap! listeners assoc key (:pixi/listeners scene {})))

(defn- build-renderer! [key scene]
  (when-let [renderer (:pixi/renderer scene)]
    (build! [key] :pixi/renderer renderer)))

(defn- build-stage! [renderer key scene]
  (when-let [stage (:pixi/stage scene)]
    (binding [*renderer* renderer]
      (build! [key] :pixi/stage stage))))

(defn- render-view [renderer stage]
  (letfn [(render [] (.render renderer stage))]
    (render)
    (on-loaded-textures #(js/requestAnimationFrame render))))

(defn mount [key scene element]
  (build-listeners! key scene)
  (when-let [renderer (build-renderer! key scene)]
    (mount-view renderer element)
    (when-let [stage (build-stage! renderer key scene)]
      (render-view renderer stage))))

(defn unmount [key]
  (when-let [renderer (:obj (@build-cache [key :pixi/renderer]))]
    (let [view (.-view renderer)]
      (when-let [parent (.-parentNode view)]
        (.removeChild parent view)))))
