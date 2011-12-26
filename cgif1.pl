                     use CGI::FormBuilder;

           # Ex 1
           # Simplest version: print out a form with 3 fields
           # This is all you need for a simple form-based app!
           my $form = CGI::FormBuilder->new(fields => [qw/name job money/],
                                            title  => 'Your Occupation');
           print $form->render(header => 1);

           # Ex 1a
           # If we have default values, for example from a DBI query,
           # we can pass these in as well:
           my $dbi_results_hashref = $sth->fetchrow_hashref;
           print $form->render(values => $dbi_values_hashref);

           # Ex 1b
           # Now we're going to modify the attributes of individual
           # fields before printing them out. Normally, FormBuilder
           # will figure this out for you automagically, but you may
           # want to customize it:

           $form->field(name  => 'job', type => 'checkbox');

           $form->field(name   => 'state', type => 'select',
                                   options => \@states);

           print $form->render(header => 1);

           # Ex 2
           # Now we decide that we want to validate certain fields.
           # To do this we pass the 'validate' option.

           my $valid_form = CGI::FormBuilder->new(
                               fields => [qw/name email/],
                               validate => {name  => 'WORD',
                                            email => 'EMAIL'}
                            );

           print $valid_form->render(header => 1);

           # Ex 3
           # Finally, we've decided that the builtin forms, while
           # nice, are not as pretty as we'd like them to be. So,
           # we construct a template via HTML::Template and specify
           # it as what to use during printing:

           my $nice_form = CGI::FormBuilder->new(
                               fields   => [qw/username password/],
                                             template => 'userinfo.html'
                           );

           # Ex 4
           # Or, if we prefer to use the Template Toolkit (TT2),
           # we can do it like this:

           my $nice_form = CGI::FormBuilder->new(
                               fields   => [qw/username password/],
                               template => {
                                     type => 'TT2',
                                     template => 'userinfo.html',
                               },
                           );

           print $nice_form->render(header => 1);

           # Ex 5
           # Of course, we can even build a complete application
           # using this module, since all fields are sticky and
           # stateful across multiple submissions. And, though
           # we're using anonymous arrayrefs []'s and hashrefs {}'s
           # above there's no reason we can't use named ones:
                        my $loopback_form = CGI::FormBuilder->new(
                                   title    => $title,
                                   fields   => \@fields,
                                   values   => \%values,
                                   validate => \%validate
                               );

           if ($loopback_form->submitted && $loopback_form->validate) {
               # We have a valid form that has been submitted
               # Here we would do stuff to use the different
               # values, and then finally print out a confirmation
               print $loopback_form->confirm;
           } else {
               print $loopback_form->render;
           }

