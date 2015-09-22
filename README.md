# MOOC

Для данного проекта нужно было построить алгоритм, предсказывающий отток пользователей. 
Заказчик - крупная образовательная интернет-платформа. На вход подавались данные об активности пользователей, на выходе была бинарная переменная.
Метрикой выступал AUC.

Лучший алгоритм, дающий 0.88 AUC на тестовой выборке, - это ансамбль из двух моделей XGBOOST и H2O-deep learning.
Были добавлены новые сгенерированные переменные, что привело к повышению качества модели на 5 п.п.
