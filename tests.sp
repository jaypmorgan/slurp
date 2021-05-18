(library tidyverse)

(defparam sepalwidth
    (head (select iris Sepal.Width)))

(print sepalwidth)
