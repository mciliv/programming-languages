  (method comparison: aNat [locals index]
    (if (= degree (degree aNat))
      {(set index degree)
       (while (= (at: aNat index) (get_digit: self index))
        {(set index (- index 1))})
       (if (< (get_digit: self index) (at: aNat index))
        {(LT)}
        {(if (> (get_digit: self index) (at: aNat index))
          {(GT)}
          {(EQUAL)})})}
      {(if (< degree (degree aNat))
        {(LT)}
        {(if (> degree (degree aNat))
          {(GT)}
          {(EQUAL)})})}))
