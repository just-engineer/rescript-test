type t = {
  inputGood: bool,
  fieldInt: int,
  fieldString: string,
}

type errorCode =
  | WrongInput(t)
  | WrongCount(t, int)
  | WrongMessage(t, string)

type validator<'a> = 'a => result<t, errorCode>

let checkInput: validator<t> = t => {
  t.inputGood ? Ok(t) : Error(WrongInput(t))
}

let checkInt: validator<t> = t => {
  if t.fieldInt > 10 {
    Ok(t)
  } else {
    Error(WrongCount(t, t.fieldInt))
  }
}

let checkString: validator<t> = t => {
  String.equal("right string", t.fieldString) ? Ok(t) : Error(WrongMessage(t, t.fieldString))
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
  | Ok(case) => Console.log(`validation success`)
  | Error(code) =>
    switch code {
    | WrongInput(case) => Console.log(`WrongInput`)
    | WrongCount(case, count) => Console.log(`WrongCount ${Belt.Int.toString(count)}`)
    | WrongMessage(case, message) => Console.log(`WrongMessage ${message}`)
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