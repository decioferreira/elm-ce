const coffee = require("coffee");

class ElmMake {
  constructor(elmFiles, params = {}) {
    this.elmEvancz = coffee.spawn(
      "./specs/helpers/elm-evancz-make.js",
      elmFiles.map(elmFile => `specs/assets/elm/${elmFile}`)
    );
    this.elmCE = coffee.spawn(
      "./lib/tasks/elm-ce-make.js",
      elmFiles.map(elmFile => `specs/assets/elm/${elmFile}`)
    );
  }

  success(modules) {
    let successMessage = new RegExp("Success!\n\n$");

    if (modules) {
      successMessage = new RegExp(`Success! Compiled ${modules} module.\n\n$`);
    }

    this.expect("stderr", "")
      .expect("stdout", successMessage)
      .expect("code", 0);

    return this;
  }

  fail(modules, message) {
    this.expect("stderr", message)
      .expect("stdout", new RegExp(`Detected problems in ${modules} module.\n\n$`))
      .expect("code", 1);

    return this;
  }

  expect() {
    this.elmEvancz = this.elmEvancz.expect.apply(this.elmEvancz, arguments);
    this.elmCE = this.elmCE.expect.apply(this.elmCE, arguments);

    return this;
  }

  end(callback) {
    Promise.all([this.elmEvancz.end()]).then(values => {
      // console.log(values);
      callback();
    });
  }
}

module.exports = (elmFiles, params = {}) => {
  const elmFilesArray = elmFiles instanceof Array ? elmFiles : [elmFiles];
  return new ElmMake(elmFilesArray, params);
};
