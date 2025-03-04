open System
open System.Runtime.InteropServices
open System.IO

[<DllImport("user32.dll")>]
extern void keybd_event(byte bVk, byte bScan, uint dwFlags, int dwExtraInfo)

let VK_MENU = 0x12uy // Alt
let VK_F4 = 0x73uy   // F4
let KEYEVENTF_KEYDOWN = 0x0000u
let KEYEVENTF_KEYUP = 0x0002u

///Симуляция вызова Alt+F4
let simulateAltF4() =
    // Нажать Alt
    keybd_event(VK_MENU, 0uy, KEYEVENTF_KEYDOWN, 0)
    // Нажать F4
    keybd_event(VK_F4, 0uy, KEYEVENTF_KEYDOWN, 0)
    // Отпустить F4
    keybd_event(VK_F4, 0uy, KEYEVENTF_KEYUP, 0)
    // Отпустить Alt
    keybd_event(VK_MENU, 0uy, KEYEVENTF_KEYUP, 0)


///Перевод строки в число (Возвращает some/none)
let string_to_numb (str: string) (numb_type: string) = 
    ///Подсчёт количества символов, не являющихся цифрами или . и ,
    let prop_string (points_count :int , not_num_count: int) symbol = 
        if symbol >= '0' && symbol <= '9' then 
            (points_count, not_num_count)
        elif symbol = ',' then 
            (points_count + 1, not_num_count + 1)
        else 
            (points_count, not_num_count + 1)

    let points_count, not_num_count = 
        List.fold (prop_string) (0, 0) (Seq.toList (str.Trim().Replace(".", ",").TrimStart('+')))
    //Проверка корректности значений кол-ва знаков, не являющихся цифрами и являющихся . и ,
    if ((numb_type.[0] = 'f' || numb_type.[1] = 'f') && points_count <= 1) || ((numb_type.[0] = 'i' || numb_type.[1] = 'i') && points_count = 0) && (points_count = not_num_count) then
        //Проверка запрашиваемого типа
        match numb_type with
        | "int" ->
            match Int32.TryParse(str) with
            | true, value -> Some(value :> obj)
            | false, _ -> None
        | "float" ->
            match Double.TryParse(str.Replace(".", ",")) with
            | true, value -> Some(value :> obj)
            | false, _ -> None
        | "uint" ->
            match Int32.TryParse(str) with
            | true, value -> Some(abs(value) :> obj)
            | false, _ -> None
        | "ufloat" ->
            match Double.TryParse(str.Replace(".", ",")) with
            | true, value -> Some(abs(value) :> obj)
            | false, _ -> None
        | _ -> None
    //Если кол-во . и , превышено
    elif ((numb_type.[0] = 'f' || numb_type.[1] = 'f') && points_count > 1) || ((numb_type.[0] = 'i' || numb_type.[1] = 'i') && points_count > 0) && (points_count = not_num_count) then 
        printfn "Количество символов '.' или ',' превышенно. "
        if (numb_type.[0] = 'f' || numb_type.[1] = 'f') then 
            printf "Ожидалось: 0 или 1. Полученно %d" points_count
        else
            printf "Ожидалось: 0. Полученно %d" points_count
        None
    //Если формат записи неверный
    else
        printfn "Неверный формат записи числа"
        None


///На основе списка вещественных чисел получить список целых чисел, отбросив дробную часть (ввод с клавиатуры). 
let task1_input() = 
    ///Ввод числа
    let input() = 
        printf "Ваше число: "
        string_to_numb (Console.ReadLine()) "float"

    ///Рекурсивное заполнение списка (хвостовая рекурсия)
    let list_format()= 
        let rec list_append(float_list: float list) = 
            let numb = input()
            match numb with
            | Some value when value = 0.0 -> List.rev float_list
            | Some value -> list_append(unbox<float> value::float_list)
            | None -> 
                printfn "Ошибка ввода"
                list_append(float_list)
        list_append []


    printfn "Вводи дробные числа. Введи число 0, чтобы прекратить:"
    let float_list = list_format()

    printf "Список из дробных чисел: ["
    List.map(fun p -> printf "%.3f " p) float_list

    printf "]\nСписок из целых чисел: ["
    List.map(fun p -> printf "%d " (int(p))) float_list //Преобразование списка дробных в список целых
    printf "]"
    0

///На основе списка вещественных чисел получить список целых чисел, отбросив дробную часть (ввод с клавиатуры). 
let task1_random() = 

    //.Запрос числа
    let rec input_num() = 
        let num = string_to_numb (Console.ReadLine()) "float"
        match num with
        | Some value -> unbox<float> value
        | None -> 
            printfn "Ошибка ввода"
            input_num()

    ///Запрос размерности
    let rec list_size() = 
        let size = string_to_numb (Console.ReadLine()) "uint"
        match size with
        | Some value -> unbox<int> value
        | None -> 
            printfn "Ошибка ввода"
            list_size()

    ///Генерация случайного списка вещественных чисел
    let generate_random count min max =
        let rand = Random()
        [ for _ in 1 .. count -> rand.NextDouble() * (max - min) + min ]

    printf "Количество чисел: "
    let count = list_size() //Кол-во чисел в списке

    printf "Минимальное число: "
    let min = input_num() //Минимальное число в диапазоне

    printf "Максимальное число: "
    let max = input_num() //Максимальное число в диапазоне
    
    let float_list = generate_random count min max //Генерация случайного списка

    printf "Список из дробных чисел: ["
    List.map(fun p -> printf "%.3f " p) float_list

    printf "]\nСписок из целых чисел: ["
    List.map(fun p -> printf "%d " (int(p))) float_list //Преобразование списка дробных в список целых
    printf "]"
    0


///Список содержит строки. Сколько из них имеет чётную длину (ручной ввод)? 
let task2() = 
    ///Ввод строки
    let string_input() =
        printf"Введи строку: "
        Console.ReadLine()

    printfn "Вводите строки. Что бы прекратить введи 0:"
    ///Рекурсивное заполнение списка(хвостовая рекурсия)
    let list_format()= 
        let rec list_append(string_list: string list) = 
            let str = string_input()
            if (str = "0") then
                string_list
            else
                list_append(str::string_list)
        list_append []
    
    printfn "Количество строк с чётным количеством символов: %d"
        (List.fold (fun ( q:int) (p:string) -> if p.Length % 2 = 0 then q+1 else q) 0 (list_format())) //Подсчёт количества строк чётной длины
    0
    

///Список содержит строки. Сколько из них имеет чётную длину (случайное заполнение)? 
let task2_rand() = 

    
    let random = Random()
    /// Генерация случайной строки указанной длины
    let generate_string length =
        let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
        let chars_array = chars.ToCharArray()
        let string_builder = System.Text.StringBuilder()
    
        for _ in 1 .. length do
            let index = random.Next(chars_array.Length)
            string_builder.Append(chars_array.[index]) |> ignore
    
        string_builder.ToString()

    /// Генерация списка случайных строк
    let string_list count  =
        [ for _ in 1 .. count -> generate_string (random.Next(1, 11)) ]

    ///Запрос размерности списка
    let rec list_size() = 
        let size = string_to_numb (Console.ReadLine()) "uint"
        match size with
        | Some value -> unbox<int> value
        | None -> 
            printfn "Ошибка ввода"
            list_size()

    printf "Количество строк: "
    let count = list_size()//Кол-во строк
    let strlist = string_list count //Список строк

    List.map(fun p -> printf "%A " p) strlist
    
    printfn "\nКоличество строк с чётным количеством символов: %d"
        (List.fold (fun ( q:int) (p:string) -> if p.Length % 2 = 0 then q+1 else q) 0 (strlist)) //Подсчёт количества строк чётной длины

    0


let main() = 
    while true do
        printfn"\nВыберите задачу из списка и введите её номер: "
        printfn"1 - Преобразование списка вещественных чисел в список целых чисел (с вводом чисел с клавиатуры)" 
        printfn"2 - Преобразование списка вещественных чисел в список целых чисел (случайное заполнение списка)" 
        printfn"3 - Подсчёт количества чётных строк. (с вводом с клавиатуры)" 
        printfn"4 - Подсчёт количества чётных строк. (случайное заполнение списка)" 
        printfn"q - Выход из программы."
        match Console.ReadLine() with
        |"1" -> task1_input() //Задание 1 (с вводом чисел с клавиатуры)
        |"2" ->  task1_random() //Задание 1 (случайное заполнение списка)
        |"3" -> task2() //Задание 2 (с вводом с клавиатуры)
        |"4" -> task2_rand()//Задание 2 (случайное заполнение списка)
        |"q" -> 
            simulateAltF4() //Закрытие программы 
            0
        |_ -> 
            printfn"Данной задачи нет в списке" 
            0


main()
