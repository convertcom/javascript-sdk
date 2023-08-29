module.exports = {
  'log-manager': {
    meta: {
      type: 'problem',
      docs: {
        description:
          'Disable nonblock statement body on LogManager methods calls'
      },
      fixable: 'code',
      schema: []
    },
    create(context) {
      return {
        // capture optional-chaining calls
        'IfStatement > :not(BlockStatement).consequent CallExpression[optional=true]':
          function (node) {
            // skip if logging is deeply nested inside a block statement
            if (
              node.parent &&
              node.parent.parent &&
              node.parent.parent.parent &&
              node.parent.parent.parent.type === 'BlockStatement'
            )
              return;
            findLogger(node);
          },
        // capture none-optional-chaining calls
        'IfStatement > :not(BlockStatement).consequent': function (node) {
          if (
            node.type === 'ExpressionStatement' &&
            node.expression.type === 'CallExpression'
          ) {
            findLogger(node.expression);
          }
        }
      };
      function findLogger(node) {
        const lookup = findProperty(node.callee);
        const callee = lookup.expression ? lookup.parent.property : lookup;
        // const method = node.callee.property
        //   ? node.callee.property.name
        //   : node.callee.name;
        // console.log(callee.name === method ? '' : callee.name, '>', method);
        if (callee.name === '_loggerManager') {
          const options = {
            node,
            message: 'LogManager method called as a nonblock statement.'
          };
          if (node.parent.parent.type === 'ConditionalExpression') {
            // no fix if logging is inside a ternary expression
          } else {
            const expression = context.sourceCode.getText(node);
            options.fix = (fixer) =>
              fixer.replaceText(node, `{ ${expression} }`);
          }
          context.report(options);
        }
      }
      function findProperty(obj) {
        if (obj.type === 'MemberExpression') {
          if (obj.object && obj.object.type !== 'ThisExpression') {
            obj = findProperty(obj.object);
          }
          if (obj.property) {
            obj = findProperty(obj.property);
          }
        }
        return obj;
      }
    }
  }
};
