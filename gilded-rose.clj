
; https://github.com/mjansen401/gilded-rose-clojure
; http://blog.8thlight.com/mike-jansen/2012/09/26/welcome-to-the-gilded-rose-in-clojure.html

(ns gilded-rose.core)

(defn update-quality [items]
  (map
    (fn[item] (cond
      (and (< (:sell-in item) 0) (= "Backstage passes to a TAFKAL80ETC concert" (:name item)))
        (merge item {:quality 0})
      (and (or (= "+5 Dexterity Vest" (:name item)) (= "Elixir of the Mongoose" (:name item))) (< (:sell-in item) 0))
        (merge item {:quality (- (:quality item) 2)})
      (and (= (:name item) "Backstage passes to a TAFKAL80ETC concert") (>= (:sell-in item) 0) (< (:sell-in item) 5))
        (merge item {:quality (+ 3 (:quality item))})
      (or (= "+5 Dexterity Vest" (:name item)) (= "Elixir of the Mongoose" (:name item)))
        (merge item {:quality (dec (:quality item))})
      (and (= (:name item) "Backstage passes to a TAFKAL80ETC concert") (>= (:sell-in item) 5) (< (:sell-in item) 10))
        (merge item {:quality (+ 2 (:quality item))})
      (= (:name item) "Aged Brie")
        (if (< (:quality item) 50)
          (merge item {:quality (inc (:quality item))})
          item)
      (= (:name item) "Backstage passes to a TAFKAL80ETC concert")
        (merge item {:quality (inc (:quality item))})
      (= 0 (:quality item)) item
      :else item))
  (map (fn [item]
    (cond
      (= "Aged Brie" (:name item))
        (merge item {:sell-in (dec (:sell-in item))})
      (= "Sulfuras, Hand of Ragnaros" (:name item))
        item
      (= "+5 Dexterity Vest" (:name item))
        (merge item {:sell-in (dec (:sell-in item))})
      (= "Backstage passes to a TAFKAL80ETC concert" (:name item))
        (merge item {:sell-in (dec (:sell-in item))})
      (= "Elixir of the Mongoose" (:name item))
        (merge item {:sell-in (dec (:sell-in item))})
      :else (merge item {:sell-in (dec (:sell-in item))})))
  items)))

(defn item [item-name, sell-in, quality]
  {:name item-name, :sell-in sell-in, :quality quality})

(defn update-current-inventory[]
  (let [inventory 
    [
      (item "+5 Dexterity Vest" 10 20)
      (item "Aged Brie" 2 0)
      (item "Elixir of the Mongoose" 5 7)
      (item "Sulfuras, Hand of Ragnaros" 0 80)
      (item "Backstage passes to a TAFKAL80ETC concert" 15 20)
    ]]
    (update-quality inventory)
    ))

; --------------
If you want to get cracking on the Clojure source then do this:

git clone git@github.com:mjansen401/gilded-rose-clojure.git

Hi and welcome to team Gilded Rose.

As you know, we are a small inn with a prime location in a prominent city ran by a friendly innkeeper named Allison. We also buy and sell only the finest goods. Unfortunately, our goods are constantly degrading in quality as they approach their sell by date.

We have a system in place that updates our inventory for us. It was developed by a no-nonsense type named Leeroy, who has moved on to new adventures. Your task is to add the new feature to our system so that we can begin selling a new category of items.

First an introduction to our system:

    All items have a sell-in value which denotes the number of days we have to sell the item

    All items have a quality value which denotes how valuable the item is

    At the end of each day our system lowers both values for every item

Pretty simple, right? Well this is where it gets interesting:

    Once the sell by date has passed, quality degrades twice as fast

    The quality of an item is never negative

    "Aged Brie" actually increases in quality the older it gets

    The quality of an item is never more than 50

    "Sulfuras", being a legendary item, never has to be sold or decreases in quality

    "Backstage passes", like aged brie, increases in quality as it's sell-in value approaches; quality increases by 2 when there are 10 days or less and by 3 when there are 5 days or less but quality drops to 0 after the concert

We have recently signed a supplier of conjured items. This requires an update to our system:

    "Conjured" items degrade in quality twice as fast as normal items

Feel free to make any changes to the update-quality method and add any new code as long as everything still works correctly. However, do not alter the item function as that belongs to the goblin in the corner who will insta-rage and one-shot you as he doesn't believe in shared code ownership.

Just for clarification, an item can never have its quality increase above 50, however "Sulfuras" is a legendary item and as such its quality is 80 and it never alters.

Original Source: http://iamnotmyself.com/2011/02/13/refactor-this-the-gilded-rose-kata/
