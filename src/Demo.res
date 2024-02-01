type errorCode =
  | WrongInput
  | WrongCount(int)
  | WrongMessage(string)

type t = {
  inputGood: bool,
  fieldInt: int,
  fieldString: string,
}

type validator<'a> = 'a => result<t, errorCode>

let checkInt: validator<t> = t => {
  if t.fieldInt > 10 {
    Ok(t)
  } else {
    Error(WrongCount(t.fieldInt))
  }
}

let checkString: validator<t> = t => {
  String.equal("right string", t.fieldString) ? Ok(t) : Error(WrongMessage(t.fieldString))
}

let checkInput: validator<t> = t => {
  t.inputGood ? Ok(t) : Error(WrongMessage(t.fieldString))
}

let valid: (result<t, errorCode>, validator<t>) => result<t, errorCode> = (res, v) => {
  switch res {
  | Ok(case) => v(case)
  | Error(e) => Error(e)
  }
}

let validation = t => t->valid(checkInput)->valid(checkInt)->valid(checkString)

let printResult: result<t, errorCode> => unit = res =>
  switch res {
  | Ok(ignored) => Console.log(`validation success`)
  | Error(code) =>
    switch code {
    | WrongInput => Console.log(`WrongInput`)
    | WrongCount(count) => Console.log(`WrongCount ${Belt.Int.toString(count)}`)
    | WrongMessage(message) => Console.log(`WrongMessage ${message}`)
    }
  }

let case = {
  inputGood: false,
  fieldInt: 10,
  fieldString: "wrong",
}
Ok(case)->validation->printResult

let case = {
  inputGood: true,
  fieldInt: 10,
  fieldString: "wrong",
}
Ok(case)->validation->printResult

let case = {
  inputGood: true,
  fieldInt: 11,
  fieldString: "wrong",
}
Ok(case)->validation->printResult

let case = {
  inputGood: true,
  fieldInt: 11,
  fieldString: "right string",
}
Ok(case)->validation->printResult