let myError = new Error("Something went really wrong.");
myError.code = -127;

throw myError;
