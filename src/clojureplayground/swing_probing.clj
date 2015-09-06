(ns clojureplayground.swing-probing
  (:import (javax.swing JFrame JLabel JButton JRadioButton JPanel)
           (java.awt.event ActionListener MouseAdapter)))

(declare event-handler)

(defn swing-example-frame []
  (let [frame (JFrame. "I am frame ;)"),
        label (JLabel. "Who'd say? I am a label!")
        button (JButton. "and I am a button")
        r-button (JRadioButton. "...here comes radio :)")
        handler (reify ActionListener
                  (actionPerformed [this e]
                    (event-handler this e)))]
    (.addActionListener button handler)
    (.addActionListener r-button handler)
    (.addMouseListener label (proxy [MouseAdapter] []
                                    (mousePressed [e]
                                 (event-handler this e))))

    (doto frame
      (.add (let [pane (JPanel.)]
              (doto pane
                (.add label)
                (.add r-button)
                (.add button))
              pane))
      (.setSize 640 480)
      (.pack)
      (.setVisible true))
    {:frame frame, :label label :button button}))

(defmulti event-handler (fn [_ e]
                          (keyword (str 'clojureplayground.swing-probing) (.. e (getSource) (getClass) (getSimpleName)))))

(derive ::JLabel ::JButton)

(defmethod event-handler ::JButton [this _]
  (println (str this "btn")))
(defmethod event-handler ::JRadioButton [this _]
  (println (str this "rbtn")))
;(defmethod event-handler :JLabel [this _]
;  (println (str this "label")))