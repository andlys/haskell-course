Чистий код може викидати винятнові ситуації, але вони можуть бути перехоплені
лише в частині коду введення-виведення. Причина в тому, що в чистому коді ми
не знаємо, чи щось колись буде викликано і чи буде взагалі, адже він лінивий
і не має чіткого порядку виконання (в той час, коли код введення-виведення
такий порядок має). Чисті функції за вмовченням ліниві, але як тільки чисті
функції починають викидати винятки, то стає важливим порядок та момент їхнього
виконання. Винятки чистого коду ми можемо перехопити лише в брудному.
Рішення просте: не треба змішувати винятки та чистий код. Використовуйте
переваги типів на зразок Either та Maybe для представлення результатів, в ході обчислення яких
можуть виникнути помилки.

Control.Exception - утворює ієрархію типів

Команди в інтерактивній оболонці:
import Control.Exception
try (print $ 5 `div` 0) :: IO (Either ArithException ())
Left divide by zero
try (print $ 5 `div` 2) :: IO (Either ArithException ())
2
Right ()

При цьому явно вказуємо тип винятка - ArithException

Тип SomeException дозволяє зловити будь-яку помилку:
try (print $ 5 `div` 0) :: IO (Either SomeException ())
Left divide by zero


try :: Exception e => IO a -> IO (Either e a)

Намагається виконати передану дію введення-виведення та повертає або
Right <результат дії> либо Left <виняток>

:t catch
catch :: Exception e => IO a -> (e -> IO a) -> IO a

Приймає в якості параметрів дію та обробник помилки (якщо при виконанні дії
генерується виняток, то викликається його обробник)

:t catches
catches :: IO a -> [Handler a] -> IO a

Приймає в якості параметрів дію та список обробників (функцій, які упаковані
конструктором даних Handler), та повертає результат дії.


> module Main where

> import Control.Exception
> import System.Environment

Не враховуємо застарілий спосіб обробки помилок, який дозволяє обробляти лише
винятки введення-виведення
> import Prelude hiding (catch)

> main :: IO ()
> printQuotients :: Integer -> Integer -> IO ()
> printQuotients a b = do
>   print $ a `div` b
>   print $ b `div` a

> params :: [String] -> (Integer, Integer)
> params [a,b] = (read a, read b)

Спроба меіну 1:
main = do
  args <- getArgs
  let (a, b) = params args
  res <- try (printQuotients a b) :: IO (Either ArithException ())
  case res of
    Left e -> putStrLn "Деление на 0!"
    Right () -> putStrLn "OK"
  putStrLn "Конец программы"


> mainAction :: [String] -> IO ()
> mainAction args = do
>   let (a, b) = params args
>   printQuotients a b

Спроба меіну 2:
main = do
  args <- getArgs
  res <- try (mainAction args) :: IO (Either SomeException ())
  case res of
    Left e -> putStrLn "Ошибка"
    Right () -> putStrLn "OK"
  putStrLn "Конец программы"

Хороший меін:
> main = do
>   args <- getArgs
>   mainAction args `catches`
>     [Handler handleArith,
>      Handler handleArgs,
>      Handler handleOthers]
>   putStrLn "Конец программы"

> handleArith :: ArithException -> IO ()
> handleArith _ = putStrLn "Деление на 0!"

> handleArgs :: PatternMatchFail -> IO ()
> handleArgs _ = putStrLn "Неверное число параметров командной строки!"

> handleOthers :: SomeException -> IO ()
> handleOthers e = putStrLn $ "Неизвестное исключение: " ++ show e



ghci> mainAction ["2","0"]
*** Exception: divide by zero
ghci> mainAction ["0","2"] `catch` handleArith
0
Деление на 0!
ghci> mainAction ["2","0"] `catch` handleArgs
*** Exception: divide by zero
ghci> mainAction ["2","0"] `catch` handleOthers
Неизвестное исключение: divide by zero
ghci> mainAction ["a", "b"] `catch` handleArgs
*** Exception: Prelude.read: no parse
ghci> mainAction ["a", "b"] `catch` handleOthers
Неизвестное исключение: Prelude.read: no parse
