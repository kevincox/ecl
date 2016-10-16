a."foo.bar".baz = 4
"k${a."foo.bar".baz}" = 9
y = a."${"foo" + "${"."}${"bar"}"}".baz
